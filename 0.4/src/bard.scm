;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       bard 0.4
;;;; Purpose:       system loader
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(require "repl.scm")

(begin (format #t "~%bard 0.4.1d2~%")
       (bard-repl))
