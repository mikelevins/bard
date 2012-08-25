;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          op.scm
;;;; Project:       Bard VM
;;;; Purpose:       definition of machine operations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define (HALT vm . args) (vm-vals vm))

(define (VAL vm v . args)
  (vmpushval! vm v)
  vm)

(define (LREF vm i j . args)
  (vmpushval! vm (lref (vm-env vm) i j))
  vm)

(define (LSETR vm i j . args)
  (vmpushval! vm (lsetter (vm-env vm) i j))
  vm)

(define (MREF vm m v . args)
  (vmpushval! vm (mref (vm-module vm) m v))
  vm)

(define (MSETR vm i j . args)
  (vmpushval! vm (msetter (vm-module vm) m v))
  vm)

(define (SREF vm . args)
  (let ((obj (vmpopval! vm))
        (snm (vmpopval! vm)))
    (vmpushval! vm (sref obj snm))
    vm))

(define (SSETR vm i j . args)
  (let ((obj (vmpopval! vm))
        (snm (vmpopval! vm)))
    (vmpushval! vm (ssetter obj snm))
    vm))

(define (DEF vm m nm v . args)
  (define-module-variable! m nm v)
  (vmpushval! vm v)
  vm)

(define (CLOSE vm . args)
  (let ((env (vm-env vm))
        (code (vmpopval! vm))
        (lambda-list (vmpopval! vm)))
    (vmpushval! vm (make-closure lambda-list code env))
    vm))

(define (PRIM p . args)
  (vmapplyprim! vm p args)
  vm)

(define (MODL vm m . args)
  (vmpushval! vm (vmgetmodule vm m))
  vm)

(define (INMOD vm m . args)
  (vmsetmodule! vm (vmgetmodule vm m))
  vm)

(define (JUMP vm dest . args)
  (vmsetcontinue! vm dest)
  vm)

(define (TJUMP vm dest . args)
  (if (vmpopval! vm) (vmsetcontinue! vm dest))
  vm)

(define (FJUMP vm dest . args)
  (if (not (vmpopval! vm)) (vmsetcontinue! vm dest))
  vm)

(define (SAVE vm . args)
  (vmpushstate! vm)
  vm)

(define (CALL vm . args)
  (let* ((fn (popval! vm))
         (lambda-list (function-lambda-list fn))
         (args (vmpopnvals! vm (compute-nvals vm lambda-list)))
         (fenv (function-env fn))
         (env (extend-function-env fenv (vmenv vm) lambda-list args)))
    (vmsetstate! vm pc: 0 code: (function-code fn) fn: fn stack: (stack vm) env: env module: (vmmodule vm))))

(define (RETURN vm . args)
  (let ((state (vmpopstate! vm)))
    (vmsetstate! vm pc: (state-pc state) code: (state-code state) fn: (state-fn state)
                 stack: (state-stack vm) env: (state-env state) module: (state-module state))
    vm))

(define $vmopname->op-table (make-table test: eq?))
(define $vmop->opname-table (make-table test: eq?))

(define-macro (defop nm)
  `(begin
     (table-set! $vmopname->op-table ',nm ,nm)
     (table-set! $vmop->opname-table ,nm ',nm)
     ',nm))

(define (opname->opfn nm)
  (table-ref $vmopname->op-table nm #f))

(define (opfn->opname opfn)
  (table-ref $vmop->opname-table opfn #f))

(defop HALT)
(defop VAL)
(defop LREF)
(defop LSETR)
(defop MREF)
(defop MSETR)
(defop SREF)
(defop SSETR)
(defop DEF)
(defop CLOSE)
(defop PRIM)
(defop MODL)
(defop INMOD)
(defop JUMP)
(defop TJUMP)
(defop FJUMP)
(defop SAVE)
(defop CALL)
(defop RETURN)
