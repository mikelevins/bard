;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; executing instructions
;;; ----------------------------------------------------------------------

(define (execute-instruction opcode instruction state)
  (let ((op (vmoperator opcode)))
    (op instruction state)))

;;; ----------------------------------------------------------------------
;;; vm main loop
;;; ----------------------------------------------------------------------

(define (vmstep state)
  (let* ((instruction (program-ref (vmstate-program state)
                                   (vmstate-pc state)))
         (opcode (instruction-opcode instruction))
         (state* (execute-instruction opcode instruction state)))
    (showvm state*)))

(define (vmexec state)
  (let* ((instruction (program-ref (vmstate-program state)
                                   (vmstate-pc state)))
         (opcode (instruction-opcode instruction))
         (state* (execute-instruction opcode instruction state)))
    (vmexec state*)))

(define (vmstart vmstate)
  (call/cc 
   (lambda (exitfn)
     (vmstate-haltfn-set! vmstate exitfn)
     (vmexec vmstate))))

(define (vmrun program)
  (let ((state (make-vmstate program 
                             0
                             (make-stack)
                             (default-environment)
                             (default-globals)
                             #f)))
    (vmstart state)))

