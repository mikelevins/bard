;;;; ***********************************************************************
;;;;
;;;; Name:          reader.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       definition of the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2024 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard.internal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (eclector.readtable:readtable-case eclector.readtable:*readtable*)
        :preserve))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the bard reader converts text to bard data representations.

(defmethod read-from-string ((str string) &optional (eof-error-p t) eof-value
                                            &key (start 0) end preserve-whitespace)
  (eclector.reader:read-from-string str eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace))

#+nil (bard.internal::read-from-string "")
#+nil (read-from-string "Foo")
#+nil (read-from-string "(cons :Foo Bar)")
