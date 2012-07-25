;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.scm
;;;; Project:       Bard VM
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (drop n ls)
  (list-tail ls n))

(define identity (lambda (x) x))
