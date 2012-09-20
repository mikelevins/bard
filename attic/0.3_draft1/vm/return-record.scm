;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          return-record.scm
;;;; Project:       Bard
;;;; Purpose:       Representation VM return records 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type return-record
  constructor: %private-make-return-record
  pc code method environment stack)

(define (make-return-record vmstate dest)
  (%private-make-return-record dest
                               (vmstate-code vmstate)
                               (vmstate-method vmstate)
                               (vmstate-environment vmstate)
                               (vmstate-stack vmstate)))

