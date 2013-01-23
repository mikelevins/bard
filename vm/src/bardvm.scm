;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (gambit-scheme)
         (standard-bindings)
         (extended-bindings)
         (inline)
         (proper-tail-calls)
         (block))

;;; ----------------------------------------------------------------------
;;; vm registers
;;; ----------------------------------------------------------------------

(define $code #f)
(define $pc 0)
(define $vals #f)
(define $env #f)

(define $prog #f)
(define $module #f)

;;; ----------------------------------------------------------------------
;;; exit continuation
;;; ----------------------------------------------------------------------

(define end #f)

;;; ----------------------------------------------------------------------
;;; main loop
;;; ----------------------------------------------------------------------

(define $banner-message "Bard VM")

(define (display-banner)
  (newline)
  (display $banner-message)
  (display " v")
  (display (bard-version-string))
  (newline))

(define-macro (fetch code) `(vector-ref ,code $pc))
(define op car)
(define args cdr)

(define (vmrun code)
  (begin
    (display-banner)
    (call/cc
     (lambda (exit)
       (initvm)
       (set! end exit)
       (set! $code code)
       (let loop ()
         (let* ((instr (fetch $code)))
           (apply (op instr) (args instr))
           (loop)))))))

