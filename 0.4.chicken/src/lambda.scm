;;;; ***********************************************************************
;;;;
;;;; Name:          lambda.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lambdas
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(define-record kernel-lambda
  parameters body env)

(define (lambda:create lambda-list body env)
  (make-kernel-lambda lambda-list body env))

(define (lambda:native-method? op)
  (or (procedure? op)
      (continuation? op)))

(define (lambda:method-native-function op) op) 

(define (make-call-env params args env)
  (if (list? params)
      (if (= (length params)
             (length args))
          (append (zip params args)
                  env)
          (error "Wrong number of arguments to kernel lambda" params args))
      (if (symbol? params)
          (cons (cons params args)
                env)
          (error "Invalid lambda list" params))))

(define (lambda:apply op args)
  (let* ((params (kernel-lambda-parameters op))
         (body (kernel-lambda-body op))
         (env (kernel-lambda-env op))
         (call-env (make-call-env params args env)))
    (kernel:eval body call-env)))

