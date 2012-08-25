;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          op.scm
;;;; Project:       Bard VM
;;;; Purpose:       vm operations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $op->name-table (make-table test: eq?))
(define $name->op-table (make-table test: eq?))

(define-macro (defop nm opfn)
  `(begin
     (define ,nm ,opfn)
     (table-set! $op->name-table ,opfn ',nm)
     (table-set! $name->op-table ',nm ,opfn)
     ',nm))

(define (op->name op)(table-ref $op->name-table op #f))
(define (name->op name)(table-ref $name->op-table name #f))

(defop VAL (lambda (vm v)(vmpushval! vm v)))
(defop LREF (lambda (vm i j)(vmpushval! vm (vmlref vm i j))))
(defop LSETR (lambda (vm i j)(vmpushval! vm (vmlsetter vm i j))))
(defop MREF (lambda (vm m v)(vmpushval! vm (vmmref vm m v))))
(defop MSETR (lambda (vm m v)(vmpushval! vm (vmmsetter vm m v))))
(defop SREF (lambda (vm)(vmpushval! vm (vmsref vm (vmpopval! vm)(vmpopval! vm)))))
(defop SSETR (lambda (vm)(vmpushval! vm (vmssetter vm (vmpopval! vm)(vmpopval! vm)))))
(defop DEF (lambda (vm)(vmpushval! vm (vmdefvar! vm (vmpopval! vm)(vmpopval! vm)))))
(defop CLOSE (lambda (vm)(vmpushval! vm (vmmakeclosure vm (vmpopval! vm)(vmpopval! vm)(vmenv vm)))))
(defop PRIM (lambda (vm p . args)(vmpushval! vm (apply-vm-prim vm p args))))
(defop MODULE (lambda (vm m)(vmpushval! vm (vmgetmodule vm m))))
(defop INMOD (lambda (vm m)(vmsetmodule! vm m)))
(defop JUMP (lambda (vm dest)(vmsetpc! vm dest)))
(defop TJUMP (lambda (vm dest)(if (vmpopval! vm)(vmsetpc! vm dest))))
(defop FJUMP (lambda (vm dest)(if (not (vmpopval! vm))(vmsetpc! vm dest))))
(defop SAVE (lambda (vm)(vmpushstate! vm)))
(defop RETURN (lambda (vm)(vmpopstate! vm)))
(defop APPLY (lambda (vm)
               (let* ((fn (vmpopval! vm))
                      (lambda-list (function-lambda-list fn))
                      (args (vmpopnvals! vm (compute-nvals lambda-list)))
                      (env (extend-function-env (function-env fn) (vmenv vm) lambda-list args)))
                 (vmsetstate! vm pc: 0 code: (function-code fn) fn: fn
                              stack: (vmstack vm) env: env module: (vmmodule vm)))))

                
                

