;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard VM
;;;; Purpose:       lexical environments
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

(define (extend-env env . bindings)
  (env-add-frame env (make-env-frame bindings)))

(define (find-in-env expr env)
  (let loop ((frames env)
             (i 0))
    (if (null? frames)
        (values #f #f)
        (let* ((frame (car frames))
               (j (vector-position-if (lambda (bnd)(eq? expr (binding-varname bnd)))
                                      frame)))
          (if j
              (values i j)
              (loop (cdr frames)(+ 1 i)))))))

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
