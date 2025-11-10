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
;;; enable redefinitions in packages common-lisp & common-lisp-user
;;; ---------------------------------------------------------------------

(eval-when (:execute :compile-toplevel :load-toplevel)
  #+sbcl
  (progn
    (sb-ext:unlock-package '#:common-lisp)
    (sb-ext:unlock-package '#:common-lisp-user)))

;;; ---------------------------------------------------------------------
;;; lists
;;; ---------------------------------------------------------------------

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

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
