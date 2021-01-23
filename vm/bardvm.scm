;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       toplevel entry point for the Bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-structure bardvm fn code pc env stack nargs instr)

(define *bardvm*)

(define (%init-bardvm #!key (fn #f)(code '())(pc 0)(env '())(stack '())(nargs 0)(instr #f))
  (set! *bardvm* (make-bardvm fn code pc env stack nargs instr)))

;;; (%init-bardvm env: '((x . 5)))
