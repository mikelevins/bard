;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the vm itself
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-vm-version "0.3.0")

(define-type vm
  id: 37C7FA3E-3C9A-45E0-B70D-CE1D252BF17B
  constructor: %private-make-vm
  env
  modules
  vals
  fn
  pc
  instr
  stack)

(define (make-vm #!key (expected-max-arg-count 32)(function #f))
  (%private-make-vm (null-env) 
                    *module-registry*
                    (make-stretchy-vector expected-max-arg-count fill-pointer: 0)
                    function
                    0
                    #f
                    '()))

;;; ---------------------------------------------------------------------
;;; vm accessors
;;; ---------------------------------------------------------------------

;;; env

(define (lvar-ref vm i j)
  (lref (vm-env vm) i j))

(define (lvar-set! vm i j v)
  (lset! (vm-env vm) i j v))

(define (vm-extend-env! vm vals)
  (vm-env-set! vm
               (extend-env (vm-env vm)
                           (make-env-frame vals))))

(define (vm-pop-env! vm)
  (vm-env-set! vm 
               (enclosing-env (vm-env vm))))

;;; modules

(define (vm-find-module mname) #f)
(define (vm-add-module mname #!optional (module #f)) #f)
(define (vm-import mname1 vname1 mname2 vname2) #f)
(define (vm-delete!-module mname) #f)
(define (mvar-ref mname varname) #f)
(define (mvar-set! mname varname val) #f)

;;; values

(define (push-val! vm v)
  (vector-push-extend! (vm-vals vm) val))

(define (pop-val! vm)
  (vector-pop! (vm-vals vm)))

(define (clear-vals! vm)
  (vector-clear! (vm-vals vm)))

(define (val vm n)
  (stretchy-vector-ref (vm-vals vm) n))

;;; functions

;;; pc

(define (vm-pc-inc! vm #!optional (increment 1))
  (vm-pc-set! vm (+ (vm-pc vm) increment)))

(define (vm-jump! vm dest)
  (vm-pc-set! vm dest))

;;; control stack

(define (make-vm-saved-state vm)
  (vector (vm-env vm)
          (vm-fn vm)
          (vm-pc vm)
          (vm-stack vm)))

(define (saved-state-env ss)
  (vector-ref ss 0))

(define (saved-state-fn ss)
  (vector-ref ss 1))

(define (saved-state-pc ss)
  (vector-ref ss 2))

(define (saved-state-stack ss)
  (vector-ref ss 3))

(define (vm-restore-from-saved! vm saved-state)
  (vm-env-set! vm (saved-state-env saved-state))
  (vm-fn-set! vm (saved-state-fn saved-state))
  (vm-pc-set! vm (saved-state-pc saved-state))
  (vm-stack-set! vm (saved-state-stack saved-state)))

(define (vm-push-state! vm)
  (vector-push-extend! (vm-stack vm)
                       (make-vm-saved-state vm)))

(define (vm-pop-state! vm)
  (vm-restore-from-saved! vm (vector-pop! (vm-stack vm))))


