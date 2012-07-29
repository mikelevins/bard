;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-vm-version "0.3.0")

(define-type vm
  id: 37C7FA3E-3C9A-45E0-B70D-CE1D252BF17B
  constructor: %private-make-vm
  pc
  instr
  code
  fn
  stack
  vals
  env
  module
  modules)


(define (pushval! vm val)(not-yet-implemented))
(define (popval! vm)(not-yet-implemented))
(define (popnvals! vm n)(not-yet-implemented))
(define (find-module vm mname)(not-yet-implemented))
(define (set-module! vm module)(not-yet-implemented))
(define (pushstate! vm destpc code fn stack env module)(not-yet-implemented))
(define (popstate! vm)(not-yet-implemented))
(define (setstate! vm pc code fn stack env module)(not-yet-implemented))
