;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-toplevel-environment '())

(define (bard:null-environment) '())

(define (bard:add-binding env var val)
  (cons (cons var val) env))

(define (bard:extend-environment env . plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to bard:extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist))
                 (more (cddr plist))
                 (new-env (cons (cons var val) env)))
            (apply bard:extend-environment `(,new-env ,@more))))))

(define (bard:find-binding env var)
  (assq var env))

(define (bard:top-level-environment)
  $bard-toplevel-environment)

(define (bard:define-variable var val)
  (let ((binding (bard:find-binding $bard-toplevel-environment var)))
    (if binding
        (set-cdr! binding val)
        (set! $bard-toplevel-environment
              (cons (cons var val)
                    $bard-toplevel-environment)))
    val))

(define (bard:set-variable! env var val)
  (let ((binding (bard:find-binding env var)))
    (if binding
        (set-cdr! binding val)
        (error "Undefined variable" var))
    val))
