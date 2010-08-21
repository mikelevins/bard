;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       definitions of the basic built-in
;;;;                datatypes
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; Common Operations on Standard Values
;;; ========================================================================

(defmethod = (x y)(cl:eql x y))

;;; ========================================================================
;;; Singleton types
;;; ========================================================================

;;; ------------------------------------------------------------
;;; Undefined
;;; ------------------------------------------------------------

(defclass undefined ()()(:metaclass singleton-class))

(defmethod print-object ((n undefined)(s stream))
  (format s "undefined"))

(defun undefined ()(make-instance 'undefined))

(defparameter undefined (undefined))

(defmethod undefined? (x)(declare (ignore x)) nil)
(defmethod undefined? ((x undefined))(declare (ignore x)) t)

(defmethod = ((x undefined) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y undefined))
  (declare (ignore x))
  nil)

(defmethod = ((x undefined) (y undefined))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Nothing
;;; ------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))

(defmethod print-object ((n nothing)(s stream))
  (format s "nothing"))

(defun nothing ()(make-instance 'nothing))

(defparameter nothing (nothing))

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x nothing))(declare (ignore x)) t)
(defun something? (x)(not (nothing? x)))

(defmethod = ((x nothing) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y nothing))
  (declare (ignore x))
  nil)

(defmethod = ((x nothing) (y nothing))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; True
;;; ------------------------------------------------------------

(defclass true ()()(:metaclass singleton-class))

(defmethod print-object ((tt true)(s stream))
  (format s "true"))

(defun true ()(make-instance 'true))

(defmethod true? (x)(declare (ignore x)) nil)
(defmethod true? ((x true))(declare (ignore x)) t)

(defmethod = ((x true) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y true))
  (declare (ignore x))
  nil)

(defmethod = ((x true) (y true))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; False
;;; ------------------------------------------------------------

(defclass false ()()(:metaclass singleton-class))

(defmethod print-object ((f false)(s stream))
  (format s "false"))

(defun false ()(make-instance 'false))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)

(defmethod = ((x false) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y false))
  (declare (ignore x))
  nil)

(defmethod = ((x false) (y false))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Integer
;;; ------------------------------------------------------------

(deftype integer ()  'cl:integer)

(defmethod integer? (x) (cl:integerp x))

(defmethod print-value ((n cl:integer)(str stream))
  (format str "~A" n))

(defmethod = ((x cl:integer) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:integer))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:integer) (y cl:integer))
  (cl:= x y))

;;; ------------------------------------------------------------
;;; Float
;;; ------------------------------------------------------------

(deftype float ()  'cl:float)

(defmethod float? (x) (cl:floatp x))

(defmethod print-value ((n cl:float)(str stream))
  (format str "~A" n))

(defmethod = ((x cl:float) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:float))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:float) (y cl:float))
  (cl:= x y))

;;; ------------------------------------------------------------
;;; Character
;;; ------------------------------------------------------------

(deftype character ()  'cl:character)

(defmethod print-value ((c cl:character)(str stream))
  (format str "\\")
  (write-char c str))

(defmethod character? (x)(declare (ignore x)) nil)
(defmethod character? ((x cl:character))(declare (ignore x)) t)

(defmethod = ((x cl:character) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:character))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:character) (y cl:character))
  (cl:char= x y))

