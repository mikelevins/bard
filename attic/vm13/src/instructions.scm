;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard VM
;;;; Purpose:       constructing and decoding vm instructions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%gen opcode . args)
   `(,opcode ,@args))

