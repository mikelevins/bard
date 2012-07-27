;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard VM
;;;; Purpose:       environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; an environment is a list of frames
;;; a frame is an alist in which each element is of the form (var . val)
;;; this arrangement means that the VM can trivially extend lexical
;;; environments nondestructively, simply by consing new frames onto the
;;; enclosing environment.
;;;
;;; compiled code does not use assoc to find variable definitions;
;;; instead, it uses a pair of indexes (i . j), where i is the index of
;;; the frame to access, and j is the index of the pair in the frame.
;;; the pairs with variables names are used (instead of discarding the
;;; variable names) to aid in debugging.
;;;
;;; this means that lexical variable access is O(n) in this implementation.
;;; I'm gambling the number of lexical variables per frame and the
;;; total number of frames will remain small enough to prove tractable.
;;; if that gamble fails to pay off, I can replace this representation
;;; with a vector-based one.
;;;
;;; each binding is of the following form:
;;;   (varname . (value . setter-function))
;;; by default the setter function is #f, making the
;;; variable immutable
;;; the compiler recognizes a binding qualifier named ":mutable"
;;; that causes it to pass the setter #t parameter when making
;;; a binding, resulting in a mutable lexical variable.
;;; in this way, immutability is the default, but it can be
;;; selectively overriden for lexical variables anywhere

(define (null-env) '())

(define (make-binding var val #!optional (setter #f))
  (let* ((binding (cons var (cons val #f)))
         (setterfn (if setter
                       (lambda (x)(set-car! (cdr binding) x))
                       #f)))
    (set-cdr! (cdr binding) setterfn)
    binding))

(define (make-env-frame) '())

(define (add-binding frame var val #!optional (setter #f))
  (cons (make-binding var val setter)
        frame))

(define (binding-var binding)
  (car binding))

(define (binding-val binding)
  (car (cdr binding)))

(define (binding-setter binding)
  (cdr (cdr binding)))

(define (enclosing-env env)
  (cdr env))

(define (extend-env env var val #!optional (setter #f))
  (cons (add-binding (make-env-frame) var val setter)
        env))

(define (in-frame? var frame)
  (let loop ((bindings frame)
             (j 0))
    (if (null? bindings)
        #f
        (if (eqv? var (car (car bindings)))
            j
            (loop (cdr bindings)
                  (+ 1 j))))))

(define (in-env? var env)
  (let loop ((frames env)
             (i 0))
    (if (null? frames)
        #f
        (let* ((frame (car frames))
               (j (in-frame? var frame)))
          (if j
           (cons i j)
           (loop (cdr frames)
                 (+ 1 i)))))))

(define (lref env i j)
  (with-exception-catcher
   (lambda (err)(error "Invalid lexical variable reference"))
   (lambda ()(binding-val (list-ref (list-ref env i) j)))))

(define (lset! env i j v)
  (with-exception-catcher
   (lambda (err)(error "Invalid lexical variable reference"))
   (lambda ()
     (let* ((binding (list-ref (list-ref env i) j))
            (setter (binding-setter binding)))
       (if binding-setter
           (binding-setter v)
           (error "Can't set an immutable variable"))))))



