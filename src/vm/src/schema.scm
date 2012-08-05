;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          schema.scm
;;;; Project:       Bard VM
;;;; Purpose:       implementation of record-like data representations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; two kinds of schema:
;;; 1. table-style schema with named fields
;;; 2. vector-style schema with element-type and count constraints

(define (slotref obj slot-name)(not-yet-implemented))
(define (slot-setter obj slot-name)(not-yet-implemented))
