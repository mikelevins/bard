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

(define (null-env) '())

(define (make-binding var val)
  (cons var val))

(define (make-env-frame bindings)
  (list bindings))

(define (add-binding frame var val)
  (cons (make-binding var val)
        frame))

(define (extend-env env plist)
  (let loop ((kvs plist)
             (bindings '()))
    (if (null? kvs)
        (cons (make-env-frame (reverse bindings))
              env)
        (if (null? (cdr kvs))
            (error (string-append "malformed variable-bindings list: "
                                  (object->string plist)))
            (let ((k (car kvs))
                  (v (cdr kvs)))
              (loop (cddr kvs)
                    (cons (make-binding k v) bindings)))))))

(define (enclosing-env env)
  (cdr env))

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
   (lambda ()(cdr (list-ref (list-ref env i) j)))))

(define (lset! env i j v)
  (with-exception-catcher
   (lambda (err)(error "Invalid lexical variable reference"))
   (lambda ()(set-cdr! (list-ref (list-ref env i) j) v))))



