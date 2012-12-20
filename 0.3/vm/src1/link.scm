;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          link.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard linker
;;;;                converts object code to executable code
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%link-instruction instruction)
  (cons (%opname->opfn (car instruction))
        (cdr instruction)))


