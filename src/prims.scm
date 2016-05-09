;;;; ***********************************************************************
;;;;
;;;; Name:          prims.scm
;;;; Project:       Bard
;;;; Purpose:       storage of vm primitives
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (make-prims)
  (make-table test: eq?))

