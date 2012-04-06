;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions-macros.scm
;;;; Project:       Bard
;;;; Purpose:       handy macros for defining functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (%define-function prototype . body)
  (let* ((rest? (position-if (lambda (x) (eq? x '&)) (cdr prototype)))
         (name (car prototype))
         (params (cdr prototype))
         (formals (%function-param-list->formal-arguments params))
         (method-signature (%function-param-list->method-signature params)))
    `(begin
       (if (##unbound? (##global-var-ref (##make-global-var ',name)))
           (##global-var-set! (##make-global-var ',name) 
                              (%make-function name: ',name)))
       (%function-add-method! ,name ',method-signature (%make-method name: ',name params: ',formals body: ',body)))))

