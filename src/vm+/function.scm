;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.scm
;;;; Project:       Bard VM
;;;; Purpose:       functions and methods
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type primitive
  id: A1A45308-AF69-4BB7-B975-7CA127118543
  constructor: %private-make-primitive
  name
  nargs
  fn)

(define (make-primitive name nargs fn)
  (%private-make-primitive name nargs fn))

(define-type function
  id: 1C9883E1-FEAC-4EEA-960F-5056AC363A01
  constructor: %private-make-function
  code
  debug-name)

(define (make-function code #!key (debug-name #f))
  (%private-make-function code debug-name))


(define-type method
  id: 40DF0A42-1D4D-4282-A931-F631D10A32F7
  constructor: %private-make-method
  lambda-list
  lambda-bindings
  code
  env
  debug-name)

(define (make-method lambda-list lambda-bindings body-code env #!key (debug-name #f))
  (%private-make-method lambda-list lambda-bindings body-code env debug-name))


(define (%funcall f . args)
  (cond
   ((procedure? f)(apply f args))
   (else (error "%funcall is not yet implemented for Bard functions and methods"))))
