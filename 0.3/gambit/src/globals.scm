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

(define (%defglobal! gtable gname gval #!key (mutable #f))
  (let ((var (%make-var gval gname mutable: mutable)))
    (table-set! gtable gname var)
    gval))

(define (%get-global globals name #!optional (default #!unbound))
  (table-ref globals name default))

(define (%set-global! globals name val)
  (table-set! globals name val))

(define (%global-mutable? globals name)
  (%var-mutable? (%get-global globals name)))

(define (%gref globals name)
  (let ((var (%get-global globals name #f)))
    (if var
        (%var-value var)
        #!unbound)))

(define (%gset! globals name val)
  (let ((var (%get-global globals name #f)))
    (if var
        ((%var-setter var) val)
        (error (str "Undefined variable " name)))))




