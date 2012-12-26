;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine operations
;;;;                using the gambit-c runtime as the vm
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; prelude
;;; ---------------------------------------------------------------------

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
;;; vm state
;;; ---------------------------------------------------------------------

(define %fn #f)
(define %setfn! #f)
(define %pc #f)
(define %setpc! #f)
(define %env #f)
(define %setenv! #f)
(define %code #f)
(define %setcode! #f)
(define %instr #f)
(define %setinstr! #f)
(define %nargs #f)
(define %setnargs! #f)
(define %vals #f)
(define %setvals! #f)
(define %stack #f)
(define %setstack! #f)
(define %globals #f)
(define %setglobals! #f)

(let ((fn #f)
      (pc 0)
      (env (%null-env))
      (code #f)
      (instr #f)
      (nargs 0)
      (vals '())
      (stack '())
      (globals (%bard-globals)))

  (set! %fn (lambda () fn))
  (set! %setfn! (lambda (f)(set! fn f)))
  (set! %pc (lambda () pc))
  (set! %setpc! (lambda (p)(set! pc p)))
  (set! %env (lambda () env))
  (set! %setenv! (lambda (e)(set! env e)))
  (set! %code (lambda () code))
  (set! %setcode! (lambda (c)(set! code c)))
  (set! %instr (lambda () instr))
  (set! %setinstr! (lambda (i)(set! instr i)))
  (set! %nargs (lambda () nargs))
  (set! %setnargs! (lambda (n)(set! nargs n)))
  (set! %vals (lambda () vals))
  (set! %setvals! (lambda (v)(set! vals v)))
  (set! %stack (lambda () stack))
  (set! %setstack! (lambda (s)(set! stack s)))
  (set! %globals (lambda () globals))
  (set! %setglobals! (lambda (g)(set! globals g))))

;;; ---------------------------------------------------------------------
;;; saved vm state
;;; ---------------------------------------------------------------------

(define-type vmstate
  constructor: %vmstate
  (fn %saved-fn)
  (pc %saved-pc)
  (env %saved-env))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

;;; values
;;; ---------------------------------------------------------------------

(define (%pushv! v)(%setvals! (cons v (%vals))))
(define (%popv!)(prog1 (car (%vals))(%setvals! (cdr (%vals)))))
(define (%pushvals! vals)(%setvals! (append vals (%vals))))
(define (%popnv! n)(prog1 (take n (%vals))(%setvals! (drop n (%vals)))))

;;; stack
;;; ---------------------------------------------------------------------

(define (%pushc! v)(%setstack! (cons v (%stack))))
(define (%popc!)(prog1 (car (%stack))(%setstack! (cdr %stack))))

;;; prims
;;; ---------------------------------------------------------------------

(define (%apply-prim p args)(apply (%prim-function p) args))

;;; function calls
;;; ---------------------------------------------------------------------

(define (%make-saved destpc)(%vmstate (%fn) destpc (%env)))

(define (%return)
  (let ((saved (%popc!)))
    (%setfn! (%saved-fn saved))
    (%setpc! (%saved-pc saved))
    (%setenv! (%saved-env saved))))

(define (%callfn nargs)
  (let ((f (%popv!))
        (args (%popnv! nargs)))
    (%setfn! f)
    (%setenv! (%make-call-env (%fn-env f) args))
    (%setpc! 0)))

;;; continuations
;;; ---------------------------------------------------------------------

(define (%makecc pc)
  (%private-make-continuation ))

(define (%setcc!)
  (let ((k (%popc!)))
    (%setfn! (%cc-fn k))
    (%setenv! (%cc-env k))
    (%setstack! (%cc-stack k))
    (%setpc! (%cc-pc k))))

(define (%fetch!)(%setinstr! (%code-ref (%code)(%pc))))
(define (%incpc!)(%setpc! (+ 1 (%pc))))
(define (%exec!)(receive (op args)(%decode (%instr))(apply op args)))

;;; environments
;;; ---------------------------------------------------------------------

(define (%lref vname)(%env-ref (%env) vname))
(define (%lset! vname val)(%env-set! (%env) vname val))
(define (%ladd! vname val)(%setenv! (%env-add (%env) vname val)))

;;; global variables
;;; ---------------------------------------------------------------------

(define (%gref vname)(%global-ref (%globals) vname))
(define (%gset! vname val)(%global-set! (%globals) vname val))
(define (%gadd! vname val)(table-set! (%globals) vname val))

;;; ---------------------------------------------------------------------
;;; vm inspection
;;; ---------------------------------------------------------------------

(define (%dumpvm)
  `((fn . ,(%fn))
    (pc . ,(%pc))
    (env . ,(%env))
    (code . ,(%code))
    (instr . ,(%instr))
    (nargs . ,(%nargs))
    (vals . ,(%vals))
    (stack . ,(%stack))
    (globals . ,(%globals))))

;;; ---------------------------------------------------------------------
;;; vm control
;;; ---------------------------------------------------------------------

(define (%loadfn fn) #f)
(define (%run) #f)
