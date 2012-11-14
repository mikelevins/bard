;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test.scm
;;;; Project:       Bard
;;;; Purpose:       bard 0.3 system tests
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; vm tests
;;; ----------------------------------------------------------------------

(define $vm (%makevm))
(define $code (->code (% CONST 2)(% GSET 'x)(% GREF 'x)))
(%setcode! $vm (%link $code))
(%showvm $vm)
