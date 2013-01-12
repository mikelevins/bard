;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-ordering.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values by magnitude
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:< (make-function debug-name: '<
                              input-types: `(,Orderable ,&)
                              output-types: `(,Boolean)))

(%add-primitive-method! bard:<
                        (list <fixnum> <fixnum>)
                        <
                        debug-name: '<)

(define bard:> (make-function debug-name: '>
                              input-types: `(,Orderable ,&)
                              output-types: `(,Boolean)))

(%add-primitive-method! bard:>
                        (list <fixnum> <fixnum>)
                        >
                        debug-name: '>)

(define bard:<= (make-function debug-name: '<=
                               input-types: `(,Orderable ,&)
                               output-types: `(,Boolean)))

(%add-primitive-method! bard:<=
                        (list <fixnum> <fixnum>)
                        <=
                        debug-name: '<=)

(define bard:>= (make-function debug-name: '>=
                               input-types: `(,Orderable ,&)
                               output-types: `(,Boolean)))

(%add-primitive-method! bard:>=
                        (list <fixnum> <fixnum>)
                        >=
                        debug-name: '>=)


