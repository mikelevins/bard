;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%null-env) '())

(define (%make-frame count)
  )

(define (%list->frame ls)
  )

(define (%frame-fill! frame start vars)
  )

(define (%frame-ref frame index #!key (default #!unbound))
  )

(define (%frame-set! frame index val)
  )

