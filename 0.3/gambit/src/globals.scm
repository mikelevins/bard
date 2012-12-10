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

(define (%gdef! gtable gname gval #!key (mutable #f))
  (table-set! gtable gname (%make-var gname gval mutable: mutable))
  gval)

(define (%gget globals name #!optional (default #!unbound))
  (table-ref globals name default))

(define (%gmutable? globals name)
  (%var-mutable? (%gget globals name)))

(define (%gref globals name)
  (let ((var (%gget globals name #f)))
    (if var
        (%var-value var)
        #!unbound)))

(define (%gsetter globals nm)
  (%var-setter (%gget globals nm)))

(define (%gset! globals nm v)
  (let* ((setter (%gsetter globals nm)))
    (if setter
        (begin
          (setter v)
          v)
        (error (str "Cannot assign to an immutable variable: " nm)))))
