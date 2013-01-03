;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-equating.scm
;;;; Project:       Bard
;;;; Purpose:       comparing for equality
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:= (make-function debug-name: '=))

(%add-primitive-method! bard:=
                        (list Anything Anything)
                        equal?
                        debug-name: '=)



