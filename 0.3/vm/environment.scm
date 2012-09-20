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

(define (env-frame env i)(list-ref env i))
(define (frame-ref fr j)(vector-ref fr j))

(define (make-binding varname val #!key (mutable #f))
  (let* ((setter.varname (cons #f varname))
         (binding (cons val setter.varname)))
    (if mutable
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
  (let ((binding (frame-ref (env-frame env i) j)))
    (if binding
        (binding-value binding)
        #!unbound)))

(define (lsetter env i j)
  (let ((binding (frame-ref (env-frame env i) j)))
    (if binding
        (binding-setter binding)
        #!unbound)))

(define (lookup env var)
  (let loop ((frames env)
             (i 0))
    (if (null? frames)
        #f
        (let* ((frame (car frames))
               (j (vector-position-if (lambda (binding)(eq? var (binding-varname binding)))
                                      frame)))
          (if j
              (list i j)
              (loop (cdr frames)
                    (+ i 1)))))))

(define (extend-environment env1 . envs)
  (let loop ((e env1)
             (more envs))
    (if (null? more)
        e
        (loop (append (car more) e)
              (cdr more)))))

(define (make-lambda-bindings params args)
  (env-add-frame (null-env)
                 (make-env-frame (map make-binding params args))))


#| tests

(define $env (null-env))
(set! $env (env-add-frame $env (make-env-frame (list (make-binding 'x 1)))))
(env-frame $env 0)
(frame-ref (env-frame $env 0) 0)
(binding-value (frame-ref (env-frame $env 0) 0))
(binding-setter (frame-ref (env-frame $env 0) 0))
(binding-varname (frame-ref (env-frame $env 0) 0))
(lref $env 0 0)
(lsetter $env 0 0)
(set! $env (env-add-frame $env (make-env-frame (list (make-binding 'y 2 mutable: #t)))))
(lref $env 0 0)
(lsetter $env 0 0)
((lsetter $env 0 0) 202)
(lref $env 0 0)
(lookup $env 'x)
(lookup $env 'y)
(lookup $env 'z)

(define $env1 (null-env))
(set! $env1 (env-add-frame $env1 (make-env-frame (list (make-binding 'x 1)))))

(define $env2 (null-env))
(set! $env2 (env-add-frame $env2 (make-env-frame (list (make-binding 'y 2)))))

(define $env3 (null-env))
(set! $env3 (env-add-frame $env3 (make-env-frame (list (make-binding 'y 202)(make-binding 'z 3)))))

(define $env4 (extend-environment $env1 $env2 $env3))
(lookup $env4 'x)
(lref $env4 2 0)
(lookup $env4 'y)
(lref $env4 0 0)
(lookup $env4 'z)
(lref $env4 0 1)

|#
