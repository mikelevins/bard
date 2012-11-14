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

(define (%link-instruction instr)
  (cons (%opcode->op (%op instr))
        (%args instr)))

(define (%link code)
  (vector-map %link-instruction code))

(define (->code . instructions)
  (list->vector instructions))
