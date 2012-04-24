;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Frame.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Frame protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Frame)

;;; frame?
;;; ---------------------------------------------------------------------

(define bard:frame? (%make-function name: 'frame?))

(%function-add-method! bard:frame? `(,Anything) (lambda (thing) (bard:false)))
(%function-add-method! bard:frame? `(,<frame>) (lambda (thing) (bard:true)))

;;; contains-key?
;;; ---------------------------------------------------------------------

(define bard:contains-key? (%make-function name: 'contains-key?))

(define (%bard-contains-key? fr thing . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (keys (%keys fr)))
    (if (any? (lambda (k) (%apply test (list thing k))) keys)
        (bard:true)
        (bard:false))))

(%function-add-method! bard:contains-key? `(,<frame> ,Anything & args) %bard-contains-key?)

;;; contains-value?
;;; ---------------------------------------------------------------------

(define bard:contains-value? (%make-function name: 'contains-value?))

(define (%bard-contains-value? fr thing . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (vals (%vals fr)))
    (if (any? (lambda (v) (%apply test (list thing v))) vals)
        (bard:true)
        (bard:false))))

(%function-add-method! bard:contains-value? `(,<frame> ,Anything & args) %bard-contains-value?)

;;; get
;;; ---------------------------------------------------------------------

(define bard:get (%make-function name: 'get))

(define (%bard-get fr thing . args)
  (let ((default (if (null? args)
                     (bard:nothing)
                     (car args))))
    (%frame-get fr thing default)))

(%function-add-method! bard:get `(,<frame> ,Anything & args) %bard-get)

;;; keys
;;; ---------------------------------------------------------------------

(define bard:keys (%make-function name: 'keys))

(define (%bard-keys fr)(%keys fr))

(%function-add-method! bard:keys `(,<frame>) %bard-keys)

;;; merge
;;; ---------------------------------------------------------------------

(define bard:merge (%make-function name: 'merge))

(define (%bard-merge fr1 fr2)(%frame-merge fr1 fr2))

(%function-add-method! bard:merge `(,<frame> ,<frame>) %bard-merge)

;;; put
;;; ---------------------------------------------------------------------

(define bard:put (%make-function name: 'put))

(define (%bard-put fr k v)(%frame-put fr k v))

(%function-add-method! bard:put `(,<frame> ,Anything ,Anything) %bard-put)

;;; vals
;;; ---------------------------------------------------------------------

(define bard:vals (%make-function name: 'vals))

(define (%bard-vals fr)(%vals fr))

(%function-add-method! bard:vals `(,<frame>) %bard-vals)

