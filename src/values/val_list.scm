;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          val_list.scm
;;;; Project:       Bard
;;;; Purpose:       random-access lists
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-macros.scm")

;;; ---------------------------------------------------------------------
;;; <ralist>
;;; ---------------------------------------------------------------------

(define %nil ra:null)
(define %null? ra:null?)
(define %list? ra:list?)
(define %cons ra:cons)
(define %car ra:car)
(define %cdr ra:cdr)
(define %list ra:list)
(define %make-list ra:make-list)
(define %length ra:length)
(define %append ra:append)
(define %reverse ra:reverse)

(define (%drop n ls)(ra:list-tail ls n))

(define %list-ref ra:list-ref)
(define %list-put ra:list-set)
(define %map ra:map)
(define %for-each ra:for-each)
(define %cons->ralist ra:random-access-list->linear-access-list)
(define %ralist->cons ra:linear-access-list->random-access-list)
