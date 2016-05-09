;;;; ***********************************************************************
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define-structure vm
  fn code pc stack env globals prims)

;;; (define $vm (make-vm #f #f 0 '() '() (make-globals) (make-prims)))

;;; ---------------------------------------------------------------------
;;; pc operations
;;; ---------------------------------------------------------------------

(define (vm-pc-inc! vm)
  (vm-pc-set! (+ 1 (vm-pc vm))))

;;; ---------------------------------------------------------------------
;;; stack operations
;;; ---------------------------------------------------------------------

(define (vm-push! vm val)
  (vm-stack-set! vm (cons val (vm-stack vm)))
  vm)

(define (vm-top vm)
  (if (null? (vm-stack vm))
      (error "stack underflow")
      (car (vm-stack vm))))

(define (vm-pop! vm)
  (let ((val (vm-top vm))
        (new-stack (cdr (vm-stack vm))))
    (vm-stack-set! vm new-stack)
    val))

;;; (define $vm (make-vm #f #f 0 '(3 2 1) '() (make-globals) (make-prims)))
;;; (vm-top $vm)
;;; (vm-push! $vm 4)
;;; (vm-pop! $vm)

;;; ---------------------------------------------------------------------
;;; globals operations
;;; ---------------------------------------------------------------------

(define (vm-global-ref vm gname)
  (table-ref (vm-globals vm) gname))

(define (vm-global-set! vm gname val)
  (table-set! (vm-globals vm) gname val))

;;; (define $vm (make-vm #f #f 0 '(3 2 1) '() (make-globals) (make-prims)))
;;; (vm-global-ref $vm 'foo)
;;; (vm-global-set! $vm 'foo "Foo")

;;; ---------------------------------------------------------------------
;;; prims operations
;;; ---------------------------------------------------------------------

(define (vm-prim-ref vm pname)
  (table-ref (vm-prims vm) pname))

(define (vm-prim-set! vm pname val)
  (table-set! (vm-prims vm) pname val))

;;; (define $vm (make-vm #f #f 0 '(3 2 1) '() (make-globals) (make-prims)))


