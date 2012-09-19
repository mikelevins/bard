;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environment.scm
;;;; Project:       Bard
;;;; Purpose:       Bard lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (null-env) '())

(define (env-ref env i j)
  (with-exception-catcher 
   (lambda (err) (error "unbound variable"))
   (lambda ()(vector-ref (list-ref env i) j))))

(define (make-binding varname val #!key (setter #f))
  (let* ((setter.varname (cons #f varname))
         (binding (cons val setter.varname)))
    (if setter
        (let ((setter (lambda (new-val)(set-car! binding new-val))))
          (set-car! setter.varname setter)))
    binding))

(define (binding-value binding)
  (car binding))

(define (binding-setter binding)
  (car (cdr binding)))

(define (binding-varname binding)
  (cdr (cdr binding)))

(define (env-add-frame env frame)
  (cons frame env))

(define (make-env-frame bindings)
  (list->vector bindings))

(define (lref env i j)
  (let ((binding (env-ref env i j)))
    (if binding
        (binding-value binding)
        #!unbound)))

(define (lsetter env i j)
  (let ((binding (env-ref env i j)))
    (if binding
        (binding-setter binding)
        #!unbound)))

(define (extend-environment env1 . envs)
  (if (null? envs)
      env1
      (extend-environment (append (car envs) env1)
                          (cdr envs))))

(define (make-lambda-bindings params args)
  (env-add-frame (null-env)
                 (make-env-frame (map make-binding params args))))
