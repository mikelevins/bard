;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          base-singletons.lisp
;;;; Project:       Bard
;;;; Purpose:       the base singletons
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :|bard|)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the base singletons are the simplest values in Bard: five unique
;;; objects: undefined, nothing, false, true, and end

(defconstant |undefined| '|undefined|)
(defconstant |nothing| nil)
(defconstant |true| '|true|)
(defconstant |false| '|false|)
(defconstant |end| '|end|)

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

(defmethod end? (x) nil)
(defmethod end? ((x (eql '|end|))) t)

