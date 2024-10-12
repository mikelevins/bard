;;;; ***********************************************************************
;;;;
;;;; Name:          reader.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       definition of the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the bard reader converts text to bard data representations.


(defparameter *bard-readtable*
  (let ((rt (copy-readtable cl:*readtable*)))
    (setf (readtable-case rt) :preserve)
    rt))

#+nil (describe *bard-readtable*)
#+nil (let ((*readtable* *bard-readtable*))
        (read-from-string "Foo"))
#+nil (let ((*readtable* *bard-readtable*))
        (read-from-string "(cons :Foo Bar)"))
#+nil (let ((*readtable* *bard-readtable*))
        (read-from-string "Foo:Bar"))
#+nil (let ((*readtable* *bard-readtable*))
        (read-from-string "::"))
#+nil (let ((*readtable* *bard-readtable*))
        (describe (read-from-string "Foo")))
