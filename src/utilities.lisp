;;;; ***********************************************************************
;;;;
;;;; Name:          utilities.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       utility functions
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; lists
;;; ---------------------------------------------------------------------

(defun starts-with? (list x)
  "Is LIST a list whose first element is X?"
  (and (consp list) (eql (first list) x)))

;;; ---------------------------------------------------------------------
;;; sequences
;;; ---------------------------------------------------------------------

(defgeneric drop (count seq &key &allow-other-keys)
  (:method ((count integer)(seq null) &key &allow-other-keys) nil)
  (:method ((count integer)(seq sequence) &key &allow-other-keys)
    (if (< count (length seq))
        (subseq seq count)
        (subseq seq (length seq)))))
