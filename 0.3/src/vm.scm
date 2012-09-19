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
  stack
  nargs)

;;; ---------------------------------------------------------------------
;;; operations on VM registers
;;; ---------------------------------------------------------------------

(define (vmpush! vmstate val)
  (vmstate-stack-set! (cons val (vmstate-stack vmstate))))

(define (vmpop! vmstate)
  (let ((val (car (vmstate-stack vmstate))))
    (vmstate-stack-set! (cdr (vmstate-stack vmstate)))
    val))

(define (vmtake! vmstate count)
  (let ((vals (take count (vmstate-stack vmstate)))
        (new-stack (drop count (vmstate-stack vmstate))))
    (vmstate-stack-set! new-stack)
    vals))

;;; ---------------------------------------------------------------------
;;; running the VM
;;; ---------------------------------------------------------------------

(define (vmreturn! vmstate return-record)
  (vmstate-pc-set! (return-record-pc return-record))
  (vmstate-code-set! (return-record-code return-record))
  (vmstate-method-set! (return-record-method return-record))
  (vmstate-environment-set! (return-record-environment return-record))
  (vmstate-stack-set! (return-record-stack return-record)))

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


