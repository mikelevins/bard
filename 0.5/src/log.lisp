;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          log.lisp
;;;; Project:       Bard
;;;; Purpose:       logging utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; with-log
;;; ---------------------------------------------------------------------

(defmacro with-log ((var path) &body body)
  `(with-open-file (,var ,path :direction :output :element-type 'character
                         :if-does-not-exist :create :if-exists :supersede)
     ,@body))
