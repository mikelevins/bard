;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard toplevel program
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-name TopLevel)
(module-compile-options main: #t)

(define (bard-run)
  (net.bardcode.Bard:main (String[])))

(bard-run)


