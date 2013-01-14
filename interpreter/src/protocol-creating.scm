;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-creating.scm
;;;; Project:       Bard
;;;; Purpose:       constructing and initializing values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:make (make-function debug-name: 'make
                                 input-types: `(,Type)
                                 restarg: 'more
                                 output-types: `(,Anything)))

(define (%bard-make-anything type . args)
  (error (str "Don't know how to make an instance of " (%as-string type))))

(%add-primitive-method! bard:make
                        `(,Anything)
                        %bard-make-anything
                        debug-name: 'make)



