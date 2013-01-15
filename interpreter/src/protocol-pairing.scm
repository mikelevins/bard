;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-pairing.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values in pairs
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")

(define bard:left (make-function debug-name: 'left
                                 signatures: (list (signature (Pair) #f (Anything)))))

(%add-primitive-method! bard:left
                        (list <pair>)
                        car
                        debug-name: 'left)

(define bard:right (make-function debug-name: 'right
                                  signatures: (list (signature (Pair) #f (Anything)))))

(%add-primitive-method! bard:right
                        (list <pair>)
                        cdr
                        debug-name: 'right)
