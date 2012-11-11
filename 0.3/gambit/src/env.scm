';;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of Bard global and lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; lexical variables
;;; ---------------------------------------------------------------------

(define %var-value car)
(define %set-var-value! set-car!)
(define %var-name cadr)
(define (%set-var-name! var nm)(set-car! (cdr var) nm))
(define %var-setter cddr)
(define (%set-var-setter! var sfn)(set-cdr! (cdr var) nm))

(define (%immutable-var-setter var val)
  (error (str "Tried to set an immutable variable: " 
              (%var-name var))))

(define (%mutable-var-setter var val)
  (%set-var-value! var val))

(define (%var-mutable? var)(eq? (%var-setter var) %mutable-var-setter))

(define (%make-var var val mutable?)
  (cons val
        (cons var
              (if mutable?
                  %mutable-var-setter
                  %immutable-var-setter))))

(define (%vset! var val)
  ((%var-setter var) var val))

(define (%vsetter var)
  (lambda (val)
    ((%var-setter var) var val)))

;;; ---------------------------------------------------------------------
;;; environment frames
;;; ---------------------------------------------------------------------

(define (%make-frame vars)
  (list->vector vars))

(define %frame-ref vector-ref)
(define %frame-set! vector-set!)

(define (%frame-var-position fr vnm)
  (vector-position vnm fr 
                   test: (lambda (nm var)
                           (eq? nm (%var-name var)))))

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
               (j (%frame-var-position fr vnm)))
          (if j
              (cons i j)
              (loop (+ i 1)
                    (cdr frames)))))))

;;; ---------------------------------------------------------------------
;;; modules
;;; ---------------------------------------------------------------------

(define-type %module
  constructor: %private-make-module
  (name %module-name)
  (entries %module-entries))

(define (%make-module mname)
  (%private-make-module mname (make-table test: eq?)))

(define (%define-var mod vname #!optional (val (%nothing)))
  (table-set! (%module-entries mod) vname val))

(define (%find-var-in-module var mod)
  (table-ref (%module-entries mod) var #!unbound))

;;; ---------------------------------------------------------------------
;;; operations on variables
;;; ---------------------------------------------------------------------

(define (%find-var vname env mod)
  (let ((lvar-indexes (%find-var-in-env vname env)))
    (if lvar-indexes
        (vector-ref (list-ref env i) j)
        (let ((mvar (%find-var-in-module vname mod)))
          (or mvar
              #!unbound)))))

(define (%lookup-var vname env mod)
  (let ((var (%find-var vname env mod)))
    (if var
        (%var-value var)
        #!unbound)))

(define (%set-var! vname val env mod)
  (let ((var (%find-var vname env mod)))
    (if var
        (%vset! var val)
        #!unbound)))

