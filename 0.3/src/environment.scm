;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environment.scm
;;;; Project:       Bard
;;;; Purpose:       bard lexical environments 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; a lexical environment is a list of frames. This way, we can easily
;;; extend an environment by pushing a new frame onto the front of
;;; an environment.
;;;
;;; a frame is a vector of bindings.

;;; ---------------------------------------------------------------------
;;; bindings
;;; ---------------------------------------------------------------------

(define-type binding
  constructor: %private-make-binding
  value
  name
  mutable?
  setter)

(define (make-default-setter binding)
  (lambda (val)
    (error (str "Can't assign to an immutable variable: "
                (binding-name binding)))))

(define (make-setter binding)
  (lambda (val)
    (binding-value-set! binding val)))

(define (make-binding name val #!key (mutable #f))
  (let ((binding (%private-make-binding val name mutable #f)))
    (if mutable
        (binding-setter-set! binding (make-setter binding))
        (binding-setter-set! binding (make-default-setter binding)))
    binding))

;;; ---------------------------------------------------------------------
;;; frames
;;; ---------------------------------------------------------------------

(define (make-environment-frame bindings)
  (list->vector bindings))

(define (environment-frame-ref frame i)
  (vector-ref frame i))

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (null-env) '())

(define (environment-add-frame env frame) 
  (cons frame env))

(define (environment-frame env i)
  (list-ref env i))

;;; returns a binding
(define (environment-ref env i j)
  (environment-frame-ref (environment-frame env i) j))

;;; (extend-environment $env `((a 0 #f)(b 1 #t)...))
(define (extend-environment env bindings)
  (let* ((maker (lambda (bnd)(make-binding (car bnd) (cadr bnd) mutable: (caddr bnd))))
         (bindings (map maker bindings)))
    (environment-add-frame env (make-environment-frame bindings))))

;;; returns a binding's value
(define (lref env i j)
  (binding-value (environment-ref env i j)))

;;; returns a binding's setter
(define (lsetter env i j)
  (binding-setter (environment-ref env i j)))

(define (find-variable-in-environment name env)
  (let env-loop ((frames env)
                 (i 0))
    (if (null? frames)
        (values #f #f)
        (let* ((frame (car frames))
               (flen (vector-length frame)))
          (let frame-loop ((j 0))
            (if (< j flen)
                (let ((binding (environment-frame-ref frame j)))
                  (if (eq? name (binding-name binding))
                      (values i j)
                      (frame-loop (+ j 1))))
                (env-loop (cdr frames)
                          (+ 1 i))))))))

