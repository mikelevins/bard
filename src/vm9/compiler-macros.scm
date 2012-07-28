;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-macros.scm
;;;; Project:       Bard VM
;;;; Purpose:       compiler macros
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (%gen opname . args)
  `(list (list ',opname ,@args)))

