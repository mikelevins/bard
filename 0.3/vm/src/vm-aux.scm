;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm-aux.scm
;;;; Project:       Bard
;;;; Purpose:       vm operations used by instructions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%push! v vm)(%setstack! vm (cons v (%stack vm))))
(define (%top vm)(car (%stack vm)))
(define (%pop! vm)(prog1 (%top vm)(%setstack! vm (cdr (%stack vm)))))

(define (%pushv! v vm)(%setvals! vm (cons v (%vals vm))))
(define (%topv vm)(car (%vals vm)))
(define (%popv! vm)(prog1 (%topv vm)(%setvals! vm (cdr (%vals vm)))))
(define (%popnv! vm n)
  (let ((old-vals (%vals vm)))
    (receive (result new-vals)(split-list old-vals n)
             (%setvals! vm new-vals)
             result)))

(define (%fetch! vm) (%setinstr! vm (%code-ref (%method-code (%fn vm))(%pc vm))))
(define (%inc! vm) (%setpc! vm (+ 1 (%pc vm))))
(define (%exec! vm) (receive (op args)(%decode (%instr vm))
                             (apply op vm args)))

(define (%make-saved vm pc)
  (%vmstate (%fn vm) pc (%env vm)))

(define (%check-argcount p argcount)
  (let ((required-count (%prim-required-argcount p)))
    (if (%prim-restargs? p)
        (assert (>= argcount required-count) (str "Not enough arguments to primitive " p))
        (assert (= argcount required-count) (str "Wrong number of arguments to primitive " p)))))

(define (%apply-prim p args)
  (%check-argcount p (length args))
  (apply (%prim-function p) args))

(define (%make-call-env vm fn arg-values)
  (let ((vars (%bind-fn-parameters fn arg-values)))
    (%merge-environments vars (%fn-env fn) (%env vm))))

(define (%callfn vm nargs)
  (let ((f (%popv! vm))
        (args (%popnv! vm nargs)))
    (%setfn! vm f)
    (%setenv! vm (%make-call-env vm (%fn-env f) args))
    (%setpc! vm 0)))

(define (%return vm)
  (let ((saved (%popc! vm)))
    (%setfn! vm (%fn saved))
    (%setenv! vm (%env saved))
    (%setpc! vm (%pc saved))))

(define (%setcc! vm)
  (let ((k (%popc! vm)))
    (%setfn! vm (%fn (%cc-vmstate k)))
    (%setenv! vm (%env (%cc-vmstate k)))
    (%setstack! vm (%cc-stack k))
    (%setpc! vm (%pc (%cc-vmstate k)))))

