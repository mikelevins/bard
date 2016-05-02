;;;; ***********************************************************************
;;;;
;;;; Name:          types-primitive.scm
;;;; Project:       Bard
;;;; Purpose:       schema <primitive>
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;; =====================================================================
;;; base schemas
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <generator>
;;; ----------------------------------------------------------------------

(define tags:$bard-generator (%next-bard-type-number))
(define <generator> (make-base-schema '<generator> tags:$bard-generator))

;;; accessors

(define generator-proc generator-instance-proc)
(define set-generator-proc! generator-instance-proc-set!)
(define generator-results generator-instance-results)
(define set-generator-results! generator-instance-results-set!)

;;; constructor

(define (make-generator vars initvals body env)
  (let* ((gen (make-generator-instance <generator> #f '()))
         (returnc #f)
         (resumec #f)
         (loopc #f)
         (args initvals)
         (yield (make-primitive
                 procedure: (lambda (v) 
                              (call/cc
                               (lambda (resume)
                                 (begin
                                   (set! resumec resume)
                                   (set-generator-results! gen (cons v (generator-results gen)))
                                   (returnc v)))))
                 debug-name: 'yield
                 required-count: 0
                 restarg: 'more))
         (resume (make-primitive
                  procedure: (lambda vals (begin (set! args vals)(loopc)))
                  debug-name: 'resume
                  required-count: 0
                  restarg: 'more))
         (env (%add-let-bindings env
                                 `((yield ,yield)
                                   (resume ,resume))))
         (method (%eval `(method ,vars ,@body) env))
         (proc (lambda ()
                 (call/cc 
                  (lambda (return)
                    (set! returnc return)
                    (if resumec
                        (resumec)
                        (let loop ()
                          (set! loopc loop)
                          (%apply method args)
                          (loop))))))))
    (set-generator-proc! gen proc)
    gen))

;;; (define $g (make-generator '(x y) '(1 1) '((yield x)(resume y (+ x y))) '()))
;;; (next $g)
;;; (generator-results $g)

(define (next g)((generator-proc g)))

