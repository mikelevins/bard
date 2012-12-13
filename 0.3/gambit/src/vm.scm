;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; prelude
;;; ---------------------------------------------------------------------

(define-macro (^ params . body)
  `(lambda ,params ,@body))

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

(define-macro (prog1 form . forms)
  `(let ((val ,form))
     (begin ,@forms)
     val))

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define-type vmstate
  constructor: %private-make-vmstate
  (code %code %setcode!)
  (pc %pc %setpc!)
  (instr %instr %setinstr!)
  (fn %fn %setfn!)
  (nargs %nargs %setnargs!)
  (env %env %setenv!)
  (globals %globals %setglobals!)
  (stack %stack %setstack!)
  (exit %exit %setexit!))

(define (%makevmstate fn env globals exitfn)
  (let* ((code (%fn-code fn))
         (nargs (%fn-nargs fn))
         (pc 0)
         (instr #f)
         (stack '()))
    (%private-make-vmstate code pc instr fn nargs env globals stack exitfn)))

(define-type savedstate
  constructor: %private-make-saved
  (code %saved-code %set-saved-code!)
  (pc %saved-pc %set-saved-pc!)
  (fn %saved-fn %set-saved-fn!)
  (env %saved-env %set-saved-env!))

(define (%make-saved vmstate)
  (vector (%fn vmstate)(%code vmstate)(%env vmstate)(%pc vmstate)))

(define (%restore-saved! vmstate saved)
  (%setfn! vmstate (%saved-fn saved))
  (%setcode! vmstate (%saved-code saved))
  (%setenv! vmstate (%saved-env saved))
  (%setpc! vmstate (%saved-pc saved)))

;;; accessors
;;; ---------------------------------------------------------------------

(define (%incvm! state)(%setpc! state (+ 1 (%pc vm))))
(define (%push! state v)(%setstack! state (cons v (%stack state))))
(define (%top state)(car (%stack state)))
(define (%pop! state)(prog1 (%top state)(%setstack! state (cdr (%stack state)))))
(define (%swap! state)(%setstack! state (cons (cadr (%stack state))(cons (car (%stack state))(cddr (%stack state))))))
(define (%popenv! state)(prog1 (car (%env state))(%setenv! state (cdr (%env state)))))

;;; ---------------------------------------------------------------------
;;; vmops
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname args . body)
  `(let ((opfn (lambda ,args ,@body)))
     (table-set! $opname->opfn-table ',opname opfn)
     (table-set! $opfn->opname-table opfn ',opname)
     ',opname))

(define (%opname->opfn opname)(table-ref $opname->opfn-table opname))
(define (%opfn->opname opfn)(table-ref $opfn->opname-table opfn 'HALT))

;;; ---------------------------------------------------------------------

(defop NOP    (s)   s)
(defop HALT   (s)   ((%exit s)))
(defop CONST  (s k) (%push! s k))
(defop CLASS  (s c) (%push! s (%gset! (%globals s) c (%make-class c))))
(defop LREF   (s l) (%push! s (%lref (%env s) l)))
(defop LSET   (s l) (%push! s (%lset! (%env s) l (%pop! s))))
(defop GREF   (s g) (%push! s (%gref (%globals s) g)))
(defop GSET   (s g) (%push! s (%gset! (%globals s) g (%pop! s))))
(defop POP    (s)   (%pop! s))
(defop PRIM   (s p) (%push! state (%apply-prim state p)))
(defop JUMP   (s d) (%setpc! state d))
(defop FJUMP  (s d) (unless (%pop! s)(%setpc! s d)))
(defop TJUMP  (s d) (when (%pop! s)(%setpc! s d)))
(defop SAVE   (s d) (let ((saved (%make-saved s)))
                      (%set-saved-pc! saved d)
                      (%push! s saved)))
(defop CALL   (s n) (%popenv! s)
                    (let ((f (%pop! s)))
                      (%setfn! s f)
                      (%setcode! s (%fn-code f))
                      (%setenv! s (%fn-env f))
                      (%setpc! s 0)
                      (%setnargs! s n)))
(defop RETURN (s)   (%swap! s)(%restore-saved! s (%pop! s)))
(defop CC     (s)   (%push! s (let ((stack (%stack state))
                                    (saved (%make-saved state)))
                                (%makeprim name: "An anonymous continuation"
                                           required-arguments: #f
                                           rest-arguments: #t
                                           opname: 'cc
                                           side-effects: #t
                                           opfn: (lambda args
                                                   (%setstack! s stack)
                                                   (for-each (lambda (arg)(%push! s arg))
                                                             args)
                                                   (%restore-saved! s saved))))))


;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define (%show-vmstate vmstate)
  (display "Bard VM v. 0.3"))

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%linkfn vmstate fn)
  (%makefn parameters: (%fn-parameters fn)
           code: (vector-for-each (lambda (instr)
                                    (if (eq? 'HALT (car instr))
                                        (cons (%exit vmstate)
                                              (cdr instr))
                                        (cons (%opname->opfn (car instr))
                                              (cdr instr))))
                                  (%fn-code fn))
           name: (%fn-name fn)
           env: (%fn-env fn)))

(define (%makevm #!key (fn #f)(env (%nullenv))(globals (%bard-globals)))
  (lambda (cmd . args)
    (call/cc 
     (lambda (exit)
       (let* ((state (%makevmstate fn env globals exit))
              (_step! #f))

         (letrec ((_link       (lambda (f)(%linkfn state f)))
                  (_fetch!     (lambda ()(%setinstr! state (%code-ref (%fn-code (%fn state))(%pc state)))))
                  (_inc!       (lambda ()(%setpc! state (+ 1 (%pc state)))))
                  (_exec!      (lambda ()(receive (op args)(%decode (%instr state))
                                                  (apply op args))))
                  (_show       (lambda ()(%show-vmstate state)))
                  (_stepfn     (lambda ()(_fetch!)(_inc!)(_exec!)))
                  (_stepshowfn (lambda ()(_fetch!)(_inc!)(_exec!)(_show)))
                  (_run!       (lambda ()(let loop ()(_step!)(loop)))))

           (case cmd
             ((load!)      (%setfn! state (_link (car args))))
             ((showsteps!) (if (car args)(set! _step! _stepfn)(set! _step! _stepshowfn)))
             ((step)       (_step!))
             ((run)        (_run!))
             (else         (error (str "Unrecognized Bard VM command: " cmd))))))))))
