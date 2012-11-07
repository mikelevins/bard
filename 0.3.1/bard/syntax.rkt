#lang racket

;;; TODO: add:
;;;  macro
;;;  method
;;;  record
;;;  variable
;;;  vector

(define-syntax def
  (syntax-rules (class macro method protocol -> record variable vector)
    ((def class classname)(display `(define classname (make-bard-class))))
    ((def protocol pname [(fname pclass ...) -> (rclass ...)] ...)(display '(define pname (make-protocol (list (make-function fname (list pclass ...)(list rclass ...)) ...)))))
    ))