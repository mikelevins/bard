;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       bard vm globals
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%bard-globals)
  (make-table test: eq?))

(define (%gset! gtable gname gval)
  (table-set! gtable gname gval)
  gval)

(define (%gref globals name)
  (table-ref gtable gname #!unbound))

