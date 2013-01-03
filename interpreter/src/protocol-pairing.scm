;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-accessing.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values in pairs
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:pair (make-function debug-name: 'pair))

(%add-primitive-method! bard:pair
                        (list Anything Anything)
                        cons
                        debug-name: 'pair)

(define bard:left (make-function debug-name: 'left))

(%add-primitive-method! bard:left
                        (list <pair>)
                        car
                        debug-name: 'left)

(define bard:right (make-function debug-name: 'right))

(%add-primitive-method! bard:right
                        (list <pair>)
                        cdr
                        debug-name: 'right)
