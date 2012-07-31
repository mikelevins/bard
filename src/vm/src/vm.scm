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
  (pc pc set-pc!)
  (instr instr set-instr!)
  (code code set-code!)
  (fn fn set-fn!)
  (stack stack set-stack!)
  (vals vals set-vals!)
  (env env set-env!)
  (module module set-module!)
  (modules modules))

(define (pushval! vm val)
  (vector-push-extend! (vm-vals vm) val))

(define (popval! vm)
  (vector-pop! (vm-vals vm)))

(define (pushvals! vm vals)
  (vector-pushlist! (vm-vals vm) vals))

(define (popnvals! vm n)
  (vector-popn! (vm-vals vm) n))

(define (make-saved-state vm)
  (vector ()))

(define (pushstate! vm destpc code fn stack env module)
  (vector-push-extend! (stack vm) (make-saved-state vm)))

(define (popstate! vm)(vector-pop! (stack vm)))

(define (setstate! vm pc code fn stack env module)
  )
