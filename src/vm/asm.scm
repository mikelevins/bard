;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          asm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard VM assembler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (%asm-form form)
  `(vector ,(car form) ,@(cdr form)))

(define-macro (%asm forms)
  (cons 'vector
        (map (lambda (f)`(%asm-form ,f))
             forms)))

(define (%disasm code)
  (let ((instrs (vector->list code)))
    (map (lambda (i)
           (let ((instr (vector->list i)))
             (cons (vm:opname (car instr))
                   (cdr instr))))
         instrs)))
