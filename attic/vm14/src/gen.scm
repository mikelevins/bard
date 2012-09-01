;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          gen.scm
;;;; Project:       Bard
;;;; Purpose:       code generators
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%gen opname . args) 
  `(,opname ,@args))


