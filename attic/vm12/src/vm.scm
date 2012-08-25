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
  vals
  )

(define (vmpushval! vm val)
  (vector-push-extend! (vm-vals vm) val))

(define (vmpopval! vm)
  (vector-pop! (vm-vals vm)))

(define (vmpushvals! vm vals)
  (vector-pushlist! (vm-vals vm) vals))

(define (popnvals! vm n)
  (vector-popn! (vm-vals vm) n))

(define (make-saved-vm destpc scode sfn sstack senv smodule)
  (vector destpc scode sfn sstack senv smodule))

(define (saved-pc state)(vector-ref state 0))
(define (saved-code state)(vector-ref state 1))
(define (saved-fn state)(vector-ref state 2))
(define (saved-stack state)(vector-ref state 3))
(define (saved-env state)(vector-ref state 4))
(define (saved-module state)(vector-ref state 5))

(define (pushstate! vm destpc scode sfn sstack senv smodule)
  (vector-push-extend! (stack vm) 
                       (make-saved-vm destpc scode sfn sstack senv smodule)))

(define (popstate! vm)(vector-pop! (stack vm)))

(define (setstate! vm spc scode sfn sstack senv smodule)
  (set-pc! vm spc)
  (set-code! vm scode)
  (set-fn! vm sfn)
  (set-stack! vm sstack)
  (set-env! vm senv)
  (set-module! vm smodule))

