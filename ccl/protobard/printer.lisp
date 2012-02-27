;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defmethod print (thing (out (eql t)))
  (print thing *standard-output*))

(defmethod print (thing (out stream))
  (print-object thing *standard-output*))

(defmethod print ((s symbol) (out stream))
  (format out "~A" (symbol-name s)))

(defmethod print ((s fset:seq)(out stream))
  (let ((count (seq:length s)))
    (if (zerop count)
        (format out "()")
        (progn
          (format out "(")
          (bard::print (seq:element s 0) out)
          (loop for i from 1 below count
               do
               (format out " ")
               (bard::print (seq:element s i) out))
          (format out ")")))))

;;; (bard::read "nothing")
;;; (bard::read "foo:bar")
;;; (symbol-package (bard::read "foo:bar"))
;;; (bard::read "#\\x")
;;; (bard::read "#\\space")
;;; (bard::print (bard::read "()") t)
;;; (bard::print (bard::read "(foo bard:bar Baz)") t)
;;; (bard::read "{}")
;;; (bard::read "{name \"Fred\" age 101}")
