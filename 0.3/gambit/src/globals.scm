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

(define (%global-ref globals name)
  (table-ref globals name #!unbound))

(define (%global-set! globals name val)
  (table-set! globals name val))




