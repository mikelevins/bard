;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-comparing.scm
;;;; Project:       Bard
;;;; Purpose:       comparing for equality
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:= (make-function debug-name: '=
                              input-types: `(,Anything ,Anything)
                              output-types: `(,Boolean)))

(%add-primitive-method! bard:=
                        (list Anything Anything)
                        equal?
                        debug-name: '=)



