;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       Bard
;;;; Purpose:       bard runtime
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass bard-runtime ()())

(defparameter *bard-character-name-table*
  `(("space" . #\space)
    ("newline" . #\newline)
    ))

(defmethod character-data-for-name (charname)(error "Unknown character name: ~A" charname))
(defmethod character-data-for-name ((charname string))
  (let ((entry (assoc charname *bard-character-name-table* :test #'equalp)))
    (and entry (cdr entry))))

(defmethod find-character-data (charname)(character-data-for-name charname))
(defmethod find-character-data ((ch cl:character)) ch)
(defmethod find-character-data ((charname string))
  (if (= (length charname) 1)
      (find-character-data (elt charname 0))
      (character-data-for-name charname)))