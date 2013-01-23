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

(define-macro (fetch code) `(vector-ref ,code $pc))

(define $code #f)
(define $pc 0)
(define $vals '())
(define $env (list (vector 0)))

(define end #f)

(define (vmrun code)
  (begin
    (newline)(display "Bard VM 0.4.0")(newline)
    (call/cc
     (lambda (exit)
       (set! end exit)
       (set! $code code)
       (let loop ()
         (let* ((instr (fetch $code)))
           (apply (car instr) (cdr instr))
           (loop)))))))

