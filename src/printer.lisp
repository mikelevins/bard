;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod print-object ((expr true-expression) stream)
  (format stream "true"))

(defmethod print-object ((expr false-expression) stream)
  (format stream "false"))

(defmethod print-object ((expr void-expression) stream)
  (format stream "void"))

(defmethod print-object ((expr symbol-expression) stream)
  (format stream "~a" (name expr)))

(defmethod print-object ((expr keyword-expression) stream)
  (format stream "~a" (name expr)))

(defmethod print-object ((expr character-expression) stream)
  (format stream "\\~c" (value expr)))

(defmethod print-object ((expr numeric-expression) stream)
  (format stream "~d" (value expr)))

(defmethod print-object ((expr ordered-sequence) stream)
  (format stream "(")
  (let ((elts (elements expr)))
    (unless (null elts)
      (labels ((print-next (es)
                 (unless (null es)
                   (let ((it (cl:first es))
                         (them (cl:rest es)))
                     (print-object it stream)
                     (cond
                       ((null them) nil)
                       ((listp them) (progn (format stream " ")
                                            (print-next them)))
                       (t (progn (format stream " : ")
                                 (print-object them stream))))))))
        (print-next elts))))
  (format stream ")"))

(defmethod print-object ((expr unordered-sequence) stream)
  (format stream "[")
  (let ((elts (elements expr)))
    (unless (null elts)
      (labels ((print-next (es)
                 (unless (null es)
                   (let ((it (cl:first es))
                         (them (cl:rest es)))
                     (print-object it stream)
                     (cond
                       ((null them) nil)
                       ((listp them) (progn (format stream " ")
                                            (print-next them)))
                       (t (progn (format stream " : ")
                                 (print-object them stream))))))))
        (print-next elts))))
  (format stream "]"))

(defmethod print-object ((expr map-expression) stream)
  (format stream "{")
  (let ((elts (elements expr)))
    (unless (null elts)
      (labels ((print-next (es)
                 (unless (null es)
                   (let ((it (cl:first es))
                         (them (cl:rest es)))
                     (print-object it stream)
                     (cond
                       ((null them) nil)
                       ((listp them) (progn (format stream " ")
                                            (print-next them)))
                       (t (progn (format stream " : ")
                                 (print-object them stream))))))))
        (print-next elts))))
  (format stream "}"))

(defmethod print-object ((expr pair-expression) stream)
  (format stream "#p")
  (call-next-method))

(defmethod print-object ((expr text-expression) stream)
  (format stream "\"~a\"" (text expr)))

(defmethod print-object ((expr ascii-text-expression) stream)
  (format stream "#a\"~a\"" (text expr)))

(defmethod print-object ((expr unicode-text-expression) stream)
  (format stream "#u\"~a\"" (text expr)))

