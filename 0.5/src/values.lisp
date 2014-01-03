;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard
;;;; Purpose:       basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; unique values
;;; ---------------------------------------------------------------------

(defconstant |bard.language|:|undefined| '|bard.language|:|undefined|)

(defmethod defined? (x)(declare (ignore x)) t)
(defmethod defined? ((x (eql '|bard.language|:|undefined|))) nil)
(defun undefined () |bard.language|:|undefined|)

(defconstant |bard.language|:|nothing| '())

(defmethod something? (x)(declare (ignore x)) t)
(defmethod something? ((x cl:null)) nil)
(defun nothing () nil)

(defconstant |bard.language|:|true| '|bard.language|:|true|)
(defconstant |bard.language|:|false| '|bard.language|:|false|)

(defmethod true? (x)(declare (ignore x)) t)
(defmethod true? ((x (eql '|bard.language|:|false|))) nil)

(defun true () |bard.language|:|true|)
(defun false () |bard.language|:|false|)


