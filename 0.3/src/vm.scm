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
  program
  environment
  globals
  stack)

(define (make-linked-code unlinked-code vmstate)
  #f)

(define (program-environment program)
  #f)

(define (make-program-environment unlinked-env vmstate)
  #f)

(define (program-globals program)
  #f)

(define (make-program-globals unlinked-globals vmstate)
  #f)

(define (program-code program)
  #f)

(define (link-program vmstate program)
  (make-program (make-linked-code (program-code program) vmstate)
                (make-program-environment (program-environment program) vmstate)
                (make-program-globals (program-globals program) vmstate)))

(define (code-ref code index)
  #f)

(define (instruction-opfn intruction)
  #f)

(define (instruction-arguments instruction)
  #f)

(define (load-program vmstate program)
  (vmstate-program-set! vmstate (link-program vmstate program))
  (vmstate-code-set! vmstate (program-code (vmstate-program vmstate)))
  (vmstate-pc-set! 0)
  vmstate)

(define (fetch! vmstate)
  (vmstate-instruction-set! vmstate (code-ref (vmstate-code vmstate) (vmstate-pc vmstate))))

(define (exec! vmstate)
  (apply (instruction-opfn (vmstate-instruction vmstate))
         (instruction-arguments (vmstate-instruction vmstate))))

(define (incpc! vmstate)
  (vmstate-pc-set! (+ 1 (vmstate-pc vmstate))))

(define (step! vmstate)
  (fetch! vmstate)
  (incpc! vmstate)
  (exec! vmstate))

(define (run! vmstate)
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


