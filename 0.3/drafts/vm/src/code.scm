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

(define (->code . instructions)
  (list->vector instructions))
