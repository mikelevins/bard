;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-vm-version "0.3.0")

(define-type vm
  id: 37C7FA3E-3C9A-45E0-B70D-CE1D252BF17B
  constructor: %private-make-vm
  pc
  code
  fn
  stack
  vals
  env
  module
  modules
  )

(define (vmpushval! vm v)
  (vector-push-extend! (vm-vals vm) v))

(define (vmpopval! vm)
  (vector-pop! (vm-vals vm)))

(define (vmlref vm i j)
  (lref (vm-env vm) i j))

(define (vmlsetter vm i j)
  (lsetter (vm-env vm) i j))

(define (vmmref vm m v)
  (mref (vm-modules vm) m v))

(define (vmmsetter vm m v)
  (msetter (vm-modules vm) m v))

(define (vmsref vm obj sname)
  (sref obj sname))

(define (vmssetter vm obj sname)
  (ssetter obj sname))

(define (vmdefvar! vm var val)
  (module-defvar! (vm-module vm) var val)
  val)

(define (vmmakeclosure vm lambda-list code env)
  (make-closure lambda-list code env))

(define (apply-vm-prim vm p args)
  (vm-check-prim-args vm p args)
  (let ((primfn (get-primfn p args)))
    (if primfn
        (apply primfn args)
        (error (string-append "Unrecognized primitive: "
                              (object->string p))))))

(define (vmgetmodule vm m)
  (find-module m (vm-modules vm)))

(define (vmsetmodule! vm m)
  (vm-module-set! vm (find-module m (vm-modules vm))))

(define (vmsetpc! vm p)
  (vm-pc-set! vm p))

(define (vmsetstate! vm #!key pc code fn stack env module)
  (vm-pc-set! vm pc)
  (vm-code-set! vm code)
  (vm-fn-set! vm fn)
  (vm-stack-set! vm stack)
  (vm-env-set! vm env)
  (vm-module-set! vm module)
  vm)

(define (vmpushstate! vm p)
  (let ((state (vector (vm-pc vm)(vm-code vm)(vm-fn vm)(vm-stack vm)(vm-env vm)(vm-module vm))))
    (vector-push-exted! (vm-stack vm) state)
    vm))

(define (vmpopstate! vm p)
  (let ((state (vector-pop! (vm-stack vm))))
    (vmsetstate! pc: (vector-ref state 0))
    (vmsetstate! code: (vector-ref state 1))
    (vmsetstate! fn: (vector-ref state 2))
    (vmsetstate! stack: (vector-ref state 3))
    (vmsetstate! env: (vector-ref state 4))
    (vmsetstate! module: (vector-ref state 5))
    vm))



