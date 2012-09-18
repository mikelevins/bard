;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard VM
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type vmstate
  halted?
  instruction
  pc
  code
  method
  environment
  globals
  stack)

(define (initvm vmstate)
  (vmstate-code-set! vmstate (method-code (vmstate-method vmstate)))
  (vmstate-pc-set! 0))

(define (loadfn vmstate method)
  (vmstate-method-set! vmstate (link-method vmstate method))
  (initvm vmstate)
  vmstate)

(define (fetch! vmstate)
  (vmstate-instruction-set! vmstate (code-ref (vmstate-code vmstate) (vmstate-pc vmstate))))

(define (exec! vmstate)
  (apply (op (vmstate-instruction vmstate))
         (args (vmstate-instruction vmstate))))

(define (incpc! vmstate)
  (vmstate-pc-set! (+ 1 (vmstate-pc vmstate))))

(define (step! vmstate)
  (fetch! vmstate)
  (incpc! vmstate)
  (exec! vmstate))

(define (run! vmstate)
  (initvm vmstate)
  (let loop ()
    (if (not (vmstate-halted? vmstate))
        (begin
          (step! vmstate)
          (loop)))))

(define (display vmstate #!optional (port #f))
  (let ((port (or port (current-output-port))))
    (newline port)
    (display "Bard VM version 0.3" port)
    (newline port)))


