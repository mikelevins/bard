;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read-table.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard read table
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass read-table ()
  ((entries :reader entries :initform (make-hash-table))))

(defparameter *bard-read-table* (make-instance 'read-table))

(defmethod reader-for ((ch character) &optional (readtable *bard-read-table*))
  (gethash ch (entries readtable)))

(defmethod set-reader-for! ((ch character)(readtable read-table)(reader function))
  (setf (gethash ch (entries readtable)) reader))

