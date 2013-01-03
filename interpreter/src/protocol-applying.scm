;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-applying.scm
;;;; Project:       Bard
;;;; Purpose:       applying functions to values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%applicable? thing)
  (or 
   (%keyed-collection? thing)
   (%primitive-method? thing)
   (%interpreted-method? thing)
   (%function? thing)
   (procedure? thing)))

(define bard:applicable?
  (make-primitive 
   procedure: %applicable?
   debug-name: 'applicable?
   required-count: 1
   restarg: #f))

(define bard:apply
  (make-primitive
   procedure: %apply
   debug-name: 'apply
   required-count: 2
   restarg: #f))
