;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nelson_actions.scm
;;;; Project:       Bard/Nelson
;;;; Purpose:       bard support for Nelson actions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $action-table (make-table test: eqv?))

(define %next-action-id #f)
(let ((_id -1))
  (set! %next-action-id
        (lambda ()
          (set! _id (+ 1 _id))
          _id)))

;;; action
;;; ----------------------------------------------------------------------

(%defspecial 'action
             (lambda (expr env)
               (let* ((body (cdr expr))
                      (method-form `(register-action (method (target parameters puzzle) ,@body))))
                 (%eval method-form env))))

;;; find-action
;;; ----------------------------------------------------------------------

(%defspecial 'find-action
             (lambda (expr env)
               (let ((id (cadr expr)))
                 (table-ref $action-table id #f))))

;;; register-action
;;; ----------------------------------------------------------------------

(%defspecial 'register-action
             (lambda (expr env)
               (let* ((method-form (cadr expr))
                      (method-obj (%eval method-form env))
                      (id (%next-action-id)))
                 (table-set! $action-table id method-obj)
                 id)))



