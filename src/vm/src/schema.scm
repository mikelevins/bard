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

;;; a bard datatype is a tuple of values.
;;; a schema is a data structure that describes such a tuple.
;;; users can create new datatypes by defining schemas.



(define (slotref obj slot-name)(not-yet-implemented))
(define (slot-setter obj slot-name)(not-yet-implemented))
