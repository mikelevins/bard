;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          names.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       interned names
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; Names
;;; ========================================================================
;;; we use CL symbols to represent Bard names

(defmethod intern-for-name ((nm string))
  (cl:intern nm :bard-names))

(defmethod intern-for-name ((nm symbol))
  (intern-for-name (symbol-name nm)))

(defmethod name ((nm string))
  (intern-for-name nm))

(defmethod name ((nm symbol))
  (intern-for-name nm))

