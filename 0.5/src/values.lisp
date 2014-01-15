;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of primitive Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; unique literals
;;; ---------------------------------------------------------------------

(defconstant |undefined| '|undefined|)
(defconstant |nothing| nil)
(defconstant |true| '|true|)
(defconstant |false| '|false|)

(defmethod defined? (x) t)
(defmethod defined? ((x (eql '|undefined|))) nil)

(defmethod print-object ((obj (eql '|undefined|))(out stream))
  (princ "undefined" out))

(defmethod print-object ((obj (eql '|true|))(out stream))
  (princ "true" out))

(defmethod print-object ((obj (eql '|false|))(out stream))
  (princ "false" out))

(defmethod something? (x) t)
(defmethod something? ((x null)) nil)

(defmethod true? (x) t)
(defmethod true? ((x null)) nil)
(defmethod true? ((x (eql '|false|))) nil)
(defmethod true? ((x (eql '|undefined|))) nil)

(defmethod false? (x) nil)
(defmethod false? ((x (eql '|false|))) t)
(defmethod false? ((x null)) t)

