;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; printer.lisp
;;;; custom bard printers
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


;;; display
;;; ---------------------------------------------------------------------

(defmethod %%display (thing &optional (stream *standard-output*))
  (format stream "~A" thing))

(defmethod %%display ((thing null) &optional (stream *standard-output*))
  (format stream "()"))

(defmethod %%display ((thing (eql t)) &optional (stream *standard-output*))
  (format stream "true"))

(defmethod %%display ((thing string) &optional (stream *standard-output*))
  (format stream "~A" thing))

(defmethod %%display ((thing cl:list) &optional (stream *standard-output*))
  (format stream "(")
  (let ((count (length thing)))
    (when (> count 0)
      (%%display (elt thing 0))
      (when (> count 1)
        (loop for i from 1 below count
           do (progn (format stream " ")
                     (%%display (elt thing i) stream))))))
  (format stream ")"))

(defmethod %%display ((thing fset:seq) &optional (stream *standard-output*))
  (format stream "#seq[")
  (let ((count (fset:size thing)))
    (when (> count 0)
      (%%display (fset:@ thing 0))
      (when (> count 1)
        (loop for i from 1 below count
           do (progn (format stream " ")
                     (%%display (fset:@ thing i) stream))))))
  (format stream "]"))

(defmethod %%display ((thing fset:map) &optional (stream *standard-output*))
  (format stream "{")
  (let* ((keys (fset:convert 'cl:list (fset:domain thing)))
         (count (length keys)))
    (when (> count 0)
      (%%display (elt keys 0) stream)
      (format stream " ")
      (%%display (fset:@ thing (elt keys 0)) stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (let ((key (elt keys i)))
                (format stream " ")
                (%%display key stream)
                (format stream " ")
                (%%display (fset:@ thing key) stream))))))
  (format stream "}"))

;;; write
;;; ---------------------------------------------------------------------

(defmethod %%write (thing &key (stream *standard-output*))
  (write thing :stream stream))

(defmethod %%write ((thing null) &key (stream *standard-output*))
  (format stream "()"))

(defmethod %%write ((thing (eql t)) &key (stream *standard-output*))
  (format stream "true"))

(defmethod %%write ((thing cl:list) &key (stream *standard-output*))
  (format stream "(")
  (let ((count (length thing)))
    (when (> count 0)
      (%%write (elt thing 0) :stream stream)
      (when (> count 1)
        (loop for i from 1 below count
           do (progn (format stream " ")
                     (%%write (elt thing i) :stream stream))))))
  (format stream ")"))

(defmethod %%write ((thing fset:seq) &key (stream *standard-output*))
  (format stream "#seq[")
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
