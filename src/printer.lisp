;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; printer.lisp
;;;; custom bard printers
;;;; ---------------------------------------------------------------------
;;;; Code from Paradigms of Artificial Intelligence Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; integrated into bardvm by mikel evins, mikel@evins.net, July 2020

(in-package :bardvm)


(defmethod bard-write (thing &key (stream *standard-output*))
  (write thing :stream stream))

(defmethod bard-write ((thing fset:seq) &key (stream *standard-output*))
  (format stream "[")
  (let ((count (fset:size thing)))
    (when (> count 0)
      (bard-write (fset:@ thing 0) :stream stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (progn (format stream " ")
                     (bard-write (fset:@ thing i) :stream stream))))))
  (format stream "]"))

(defmethod bard-write ((thing fset:map) &key (stream *standard-output*))
  (format stream "{")
  (let* ((keys (fset:convert 'cl:list (fset:domain thing)))
         (count (length keys)))
    (when (> count 0)
      (bard-write (elt keys 0) :stream stream)
      (format stream " ")
      (bard-write (fset:@ thing (elt keys 0)) :stream stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (let ((key (elt keys i)))
                (format stream " ")
                (bard-write key :stream stream)
                (format stream " ")
                (bard-write (fset:@ thing key) :stream stream))))))
  (format stream "}"))
