;;;; ***********************************************************************
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define *bard-prompt* "bard> ")

(define (bard:repl #!key (debug #f))
  (gc-report-set! #f)
  (newline)
  (display $bard-version-string)
  (newline)
  )





