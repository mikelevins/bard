;;;; ***********************************************************************
;;;;
;;;; Name:          reader.scm
;;;; Project:       Bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (reader:string->object str)
  (call-with-input-string str (lambda (in)(read in))))

