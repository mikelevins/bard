;;;; ***********************************************************************
;;;;
;;;; Name:          singleton-values.lisp
;;;; Project:       the Bard language
;;;; Purpose:       unique constants
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard.core)

(defclass |end| (cl-singleton-mixin:singleton-mixin)())

(defmethod print-object ((object |end|) out)
  (princ "end" out))

(defclass |false| (cl-singleton-mixin:singleton-mixin)())

(defmethod print-object ((object |false|) out)
  (princ "false" out))

(defparameter |end| (make-instance '|end|))
(defparameter |nothing| nil)
(defparameter |true| t)
(defparameter |false| (make-instance '|false|))

(defun |true| () cl:T)
(defun |false| () (make-instance '|false|))
(defun |nothing| () cl:NIL)
(defun |end| () (make-instance '|end|))

(defmethod |true?| (thing)(declare (ignore thing)) cl:t)
(defmethod |true?| ((thing |false|))(declare (ignore thing)) cl:NIL)
(defmethod |true?| ((thing cl:NULL))(declare (ignore thing)) cl:NIL)

(defmethod |false?| (thing)(declare (ignore thing)) cl:NIL)
(defmethod |false?| ((thing |false|))(declare (ignore thing)) cl:T)
(defmethod |false?| ((thing cl:NULL))(declare (ignore thing)) cl:T)

(defmethod |something?| (thing)(declare (ignore thing)) cl:t)
(defmethod |something?| ((thing |false|))(declare (ignore thing)) cl:T)
(defmethod |something?| ((thing cl:NULL))(declare (ignore thing)) cl:NIL)

(defmethod |nothing?| (thing)(declare (ignore thing)) cl:NIL)
(defmethod |nothing?| ((thing |false|))(declare (ignore thing)) cl:NIL)
(defmethod |nothing?| ((thing cl:NULL))(declare (ignore thing)) cl:T)

(defmethod |end?| (thing)(declare (ignore thing)) cl:NIL)
(defmethod |end?| ((thing |end|))(declare (ignore thing)) cl:T)
