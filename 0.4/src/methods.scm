;;;; ***********************************************************************
;;;;
;;;; Name:          methods.scm
;;;; Project:       Bard
;;;; Purpose:       representation of methods
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define-structure native-method
  procedure)

(define-structure method
  parameters body env)

(define (make-call-env params args env)
  (if (list? params)
      (if (= (length params)
             (length args))
          (append (zip params args)
                  env)
          (error "Wrong number of arguments to method" params args))
      (if (symbol? params)
          (cons (cons params args)
                env)
          (error "Invalid lambda list" params))))

(define (apply-method op args)
  (let* ((params (method-parameters op))
         (body (method-body op))
         (env (method-env op))
         (call-env (make-call-env params args env)))
    (kernel:eval body call-env)))
