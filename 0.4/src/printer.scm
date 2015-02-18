;;;; ***********************************************************************
;;;;
;;;; Name:          printer.scm
;;;; Project:       Bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (printer:object->string obj)
  (if (eqv? obj #!void)
      ""
      (object->string obj)))

