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

;;; (define $vm (make-vm #f #f 0 '() '() (make-globals) #f))

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

;;; (define $vm (make-vm #f #f 0 '(3 2 1) '() (make-globals) #f))
;;; (vm-top $vm)
;;; (vm-push! $vm 4)
;;; (vm-pop! $vm)

