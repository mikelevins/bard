;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; lexical variables
;;; ---------------------------------------------------------------------

(define %lvar-value car)
(define %set-lvar-value! set-car!)
(define %lvar-name cadr)
(define (%set-lvar-name! lvar nm)(set-car! (cdr lvar) nm))
(define %lvar-setter cddr)
(define (%set-lvar-setter! lvar sfn)(set-cdr! (cdr lvar) nm))

(define (%immutable-lvar-setter lvar val)
  (error (str "Tried to set an immutable variable: " 
              (%lvar-name lvar))))

(define (%mutable-lvar-setter lvar val)
  (%set-lvar-value! lvar val))

(define (%lvar-mutable? lvar)(eq? (%lvar-setter lvar) %mutable-lvar-setter))

(define (%make-lvar var val mutable?)
  (cons val
        (cons var
              (if mutable?
                  %mutable-lvar-setter
                  %immutable-lvar-setter))))

(define %lref %lvar-value)

(define (%lset! lvar val)
  ((%lvar-setter lvar) lvar val))

(define (%lsetter lvar)
  (lambda (val)
    ((%lvar-setter lvar) lvar val)))

;;; ---------------------------------------------------------------------
;;; environment frames
;;; ---------------------------------------------------------------------

(define (%make-frame lvars)
  (list->vector lvars))

(define %frame-ref vector-ref)
(define %frame-set! vector-set!)

(define (%frame-lvar-position fr vnm)
  (vector-position vnm fr 
                   test: (lambda (nm lvar)
                           (eq? nm (%lvar-name lvar)))))

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (%null-env) '())
(define (%add-frame env fr)(cons fr env))
(define (%drop-frame env)(cdr env))

(define (%find-var-in-env vnm env)
  (let loop ((i 0)
             (frames env))
    (if (null? frames)
        #f
        (let* ((fr (car frames))
               (j (%frame-lvar-position fr vnm)))
          (if j
              (cons i j)
              (loop (+ i 1)
                    (cdr frames)))))))
