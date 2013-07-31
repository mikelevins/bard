;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          program.scm
;;;; Project:       Bard
;;;; Purpose:       representation of vm programs
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define program vector)
(define program-length vector-length)

(define (make-program code-vector) 
  code-vector)

(define (program-ref program index)
  (vector-ref program index))

(define (asm . instructions)
  (list->vector
   (map (lambda (i)
          (let* ((opnm (car i))
                 (opcode (opcode opnm))
                 (opargs (cdr i)))
            (cons opcode opargs)))
        instructions)))

(define (link . code)
  (vector-map (lambda (i)
                (let ((opc (car i))
                      (op (vmoperator opc))
                      (opargs (cdr i)))
                  (cons op opargs)))
              code))
