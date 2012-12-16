;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          assemble.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard assembler
;;;;                output is symbolic code vectors
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%assemble code)
  (list->vector code))
