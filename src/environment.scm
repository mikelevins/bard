;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environment.scm
;;;; Project:       bard
;;;; Purpose:       representation of bard environments
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (make-environment) '())

(define (environment plist)
  (plist->alist plist))

(define (add-binding env key val)
  (cons (cons key val)
        env))

(define (extend-environment env bindings)
  (let loop ((bindings bindings)
             (env env))
    (if (null? bindings)
        env
        (if (null? (cdr bindings))
            (error "Malformed bindings list" bindings)
            (let ((key (car bindings))
                  (val (cadr bindings))
                  (more (cddr bindings)))
              (loop more (add-binding env key val)))))))

(define (find-binding env key)
  (let ((binding (assq key env)))
    (if binding
        binding
        #f)))

(define (bound? key env)
  (and (find-binding env key) #t))

(define (make-standard-environment)
  (let ((env (make-environment)))
    env))