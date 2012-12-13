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

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define (%makevmstate fn env globals exitfn)
  (let* ((code (%fn-code fn))
         (pc 0)
         (instr #f)
         (stack '()))
    (vector code pc instr fn env globals stack exitfn)))

(define (%make-saved-vmstate vmstate)(vector-copy vmstate))
(define (%copy-vmstate! src dest)(subvector-move! src 0 (vector-length src) dest 0))

;;; ---------------------------------------------------------------------
;;; vmstate accessors
;;; ---------------------------------------------------------------------

(define (%code vmstate)(vector-ref vmstate 0))
(define (%pc vmstate)(vector-ref vmstate 1))
(define (%instr vmstate)(vector-ref vmstate 2))
(define (%fn vmstate)(vector-ref vmstate 3))
(define (%env vmstate)(vector-ref vmstate 4))
(define (%globals vmstate)(vector-ref vmstate 5))
(define (%stack vmstate)(vector-ref vmstate 6))
(define (%exit vmstate)(vector-ref vmstate 7))

(define (%setcode! vmstate v)(vector-set! vmstate 0 v))
(define (%setpc! vmstate v)(vector-set! vmstate 1 v))
(define (%setinstr! vmstate v)(vector-set! vmstate 2 v))
(define (%setfn! vmstate v)(vector-set! vmstate 3 v))
(define (%setenv! vmstate v)(vector-set! vmstate 4 v))
(define (%setglobals! vmstate v)(vector-set! vmstate 5 v))
(define (%setstack! vmstate v)(vector-set! vmstate 6 v))
(define (%setexit! vmstate v)(vector-set! vmstate 7 v))

;;; ---------------------------------------------------------------------
;;; vm ops
;;; ---------------------------------------------------------------------

;;; increment pc
(define (%incvm! state) #f)

;;; value-stack ops
(define (%push! state v) #f)
(define (%pop! state) #f)
(define (%top state) #f)

;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define (%show-vmstate vmstate)
  (display "Bard VM v. 0.3"))

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%linkfn fn opvec)
  (%makefn parameters: (%fn-parameters fn)
           code: (vector-for-each (lambda (instr)
                                    (cons (vector-ref opvec (vector-position (car instr) opvec))
                                          (cdr instr)))
                                  (%fn-code fn))
           name: (%fn-name fn)
           env: (%fn-env fn)))

(define (%makevm #!key (fn #f)(env (%nullenv))(globals (%bard-globals)))
  (lambda (cmd . args)
    (call/cc 
     (lambda (exit)
       (let* ((state (%makevmstate fn env globals exit))
              (_step! #f))

         (letrec ((NOP    (^ ()     state))
                  (HALT   (^ ()     (exit state)))
                  (CONST  (^ (k)    (%push! state k)))
                  (CLASS  (^ (nm)   (%push! state (%gset! (%globals state) nm (%make-class nm)))))
                  (LREF   (^ (nm)   (%push! state (%lref (%env state) nm))))
                  (LSET   (^ (nm)   (%lset! (%env state) nm (%top state))))
                  (GREF   (^ (g)    (%push! state (%gref (%env state) g))))
                  (GSET   (^ (g)    (%push! state (%gset! (%globals state) g (%top state)))))
                  (POP    (^ ()     (%pop! state)))
                  (PRIM   (^ ()     (%push! state (%apply-prim state))))
                  (JUMP   (^ (d)    (%setpc! state d)))
                  (FJUMP  (^ (d)    (unless (%pop! state)(%setpc! state d))))
                  (TJUMP  (^ (d)    (when (%pop! state)(%setpc! state d))))
                  (CALL   (^ ()     (%push! state (%make-saved-vmstate state))
                                    (%setfn! state (%pop! state))
                                    (%seten! state (%activate-method-env (%fn-env (%fn state)) (%env state)))
                                    (%setpc! state 0)))
                  (RETURN (^ ()     (let ((stack (%stack state)))
                                      (%copy-vmstate! (%pop! state) state)
                                      (%setstack! state stack)
                                      (%incvm! state))))
                  (CC     (^ ()     (error "CC not yet implemented")))
                  (SETCC  (^ ()     (%setstack! state (%top state))))

                  
                  ($_vmops (vector NOP HALT CONST CLASS LREF LSET GREF GSET POP PRIM JUMP FJUMP TJUMP CALL RETURN CC SETCC))

                  (_link       (lambda (f)(%linkfn f $_vmops)))
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
