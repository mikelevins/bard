;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; printer.lisp
;;;; custom bard printers
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


(defmethod %%write (thing &key (stream *standard-output*))
  (write thing :stream stream))

(defmethod %%write ((thing null) &key (stream *standard-output*))
  (format stream "()"))

(defmethod %%write ((thing (eql t)) &key (stream *standard-output*))
  (format stream "true"))

(defmethod %%write ((thing fset:seq) &key (stream *standard-output*))
  (format stream "[")
  (let ((count (fset:size thing)))
    (when (> count 0)
      (%%write (fset:@ thing 0) :stream stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (progn (format stream " ")
                     (%%write (fset:@ thing i) :stream stream))))))
  (format stream "]"))

(defmethod %%write ((thing fset:map) &key (stream *standard-output*))
  (format stream "{")
  (let* ((keys (fset:convert 'cl:list (fset:domain thing)))
         (count (length keys)))
    (when (> count 0)
      (%%write (elt keys 0) :stream stream)
      (format stream " ")
      (%%write (fset:@ thing (elt keys 0)) :stream stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (let ((key (elt keys i)))
                (format stream " ")
                (%%write key :stream stream)
                (format stream " ")
                (%%write (fset:@ thing key) :stream stream))))))
  (format stream "}"))
