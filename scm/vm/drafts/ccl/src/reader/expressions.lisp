;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          expressions.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard syntactic expressions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass expression ()())

(defclass nothing-expression (expression)())

(defmethod print-object ((nexp nothing-expression)(s stream))
  (print-unreadable-object (nexp s :type t :identity t)))

(defclass boolean-expression (expression)())

(defclass true-expression (boolean-expression)())

(defmethod print-object ((texp true-expression)(s stream))
  (print-unreadable-object (texp s :type t :identity t)))

(defclass false-expression (boolean-expression)())

(defmethod print-object ((fexp false-expression)(s stream))
  (print-unreadable-object (fexp s :type t :identity t)))

(defclass name-expression (expression)
  ((name :reader name :initarg :name)))

(defmethod print-object ((nm name-expression)(s stream))
  (print-unreadable-object (nm s :type t)
    (format s "~A" (name nm))))