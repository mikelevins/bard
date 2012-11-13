;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          code.scm
;;;; Project:       Bard
;;;; Purpose:       representation of vm code
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define %code-ref vector-ref)

(define (%link vm code)
  (let ((len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (let ((instr (%code-ref code i)))
            (set-car! instr (%opname->op vm (car instr)))
            (loop (+ 1 i)))
          code))))

(define (%code . instructions)
  (list->vector instructions))
