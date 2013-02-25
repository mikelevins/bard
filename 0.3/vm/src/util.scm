;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          util.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; vector utils
;;; ----------------------------------------------------------------------

(define (vector-map fn vec)
  (let ((elts (vector->list vec)))
    (list->vector (map fn elts))))
