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

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; vm registers
;;; ----------------------------------------------------------------------


(define $pc 0)
(define $code #f)
(define $program #f)

;;; the size of $vals dictates the maximum number of arguments and of
;;; return values
(define $vals (make-stack 1024))

(define $env #f)
(define $globals #f)

(define $instructions #f)

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

(define (exec!)
  (let* ((instr (vector-ref $code $pc))
         (opcode (vector-ref instr 0))
         (opfn (vector-ref $instructions opcode)))
    (opfn)
    (exec!)))

(define (vmrun)
  (call/cc
   (lambda (exit)
     (set! $instructions (instructions-vector exit))
     (exec!))))

