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

(define-type %environment
  id: D15B2526-146E-4263-9C2B-C62FBB9B115C
  constructor: %private-make-environment
  (bindings %environment-bindings %set-environment-bindings!))

(define (%null-environment) (%private-make-environment '()))

(define $bard-toplevel-environment (%null-environment))

(define (%add-binding env var val)
  (%private-make-environment (cons (cons var val)
                                   (%environment-bindings env))))

(define (%find-binding env var)
  (assq var (%environment-bindings env)))

(define (%binding-value binding)(cdr binding))
(define (%set-binding-value! binding val)(set-cdr! binding val))

(define (%extend-environment env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%extend-environment (%add-binding env var val) (cddr plist))))))


(define (%set-binding! env var val)
  (let ((binding (%find-binding env var)))
    (if binding
        (%set-binding-value! binding val)
        (%set-environment-bindings! env 
                                    (cons (cons var val)
                                          (%environment-bindings env))))))

(define (%set-bindings! env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%set-bindings-aux! (%set-binding! env var val) (cddr plist))))))

(define (%top-level-environment)
  $bard-toplevel-environment)

(define (%define-variable var val #!optional (env (%top-level-environment)))
  (%set-binding! env var val)
  val)

(define (%set-variable! var val #!optional (env (%top-level-environment)))
  (let ((binding (%find-binding env var)))
    (if binding
        (%set-binding! env var val)
        (error "Undefined variable" var))
    val))
