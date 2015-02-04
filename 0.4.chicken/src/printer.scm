;;;; ***********************************************************************
;;;;
;;;; Name:          printer.scm
;;;; Project:       Bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(use extras)

(define (printer:object->string obj)
  (format #f "~S" obj))

