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

(include "../c_api/error_macros.scm")

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


;;; C API
;;; ----------------------------------------------------------------------

(c-declare "#import <Foundation/Foundation.h>")

(define (api:run-action id target parameters puzzle)
  (reporting-errors
   (let* ((actionfn (table-ref $action-table id #f))
          (changes (if actionfn
                       (let ((tgt (objc:NSMutableDictionary->frame target))
                             (parms (objc:NSMutableDictionary->frame parameters))
                             (pzl (objc:NSMutableDictionary->frame puzzle)))
                         (%funcall actionfn tgt parms pzl))
                       (%make-frame `(error: #t message: (string-append "Unrecognized action ID: "
                                                                        (object->string id)))))))
     (objc:frame->NSMutableDictionary changes))))

(c-define (c:run-action id target parameters puzzle) 
          (int (pointer "NSMutableDictionary")(pointer "NSMutableDictionary")(pointer "NSMutableDictionary")) 
          (pointer "NSMutableDictionary") "bard_run_action" ""
          (api:run-action id target parameters puzzle))

