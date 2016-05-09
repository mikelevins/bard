;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;;                this file is also compiled last by Gambit, giving its
;;;;                name ("bard")the library loader for the built library
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
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





