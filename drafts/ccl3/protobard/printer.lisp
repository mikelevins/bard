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
  (print-object thing out))

(defmethod print ((x null) (out stream))
  (format out "()"))

(defmethod print ((s symbol) (out stream))
  (format out "~A" (symbol-name s)))

(defmethod print ((s cl:cons)(out stream))
  (format out "(")
  (when (not (null s))
    (print (car s) out))
  (when (not (null (cdr s)))
    (loop for x in (cdr s)
         do
         (format out " ")
         (print x out)))
  (format out ")"))

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
               (print (seq:element s i) out))
          (format out ")")))))

(defmethod print ((s fset:map)(out stream))
  (let* ((keys (as 'fset:seq (map:keys s)))
         (count (seq:length keys)))
    (if (zerop count)
        (format out "{}")
        (progn
          (format out "{")
          (bard::print (seq:element keys 0) out)
          (format out " ")
          (bard::print (map:get s (seq:element keys 0)) out)
          (loop for i from 1 below count
               do
               (format out " ")
               (bard::print (seq:element keys i) out)
               (format out " ")
               (bard::print (map:get s (seq:element keys i)) out))
          (format out "}")))))

;;; (bard::read "nothing")
;;; (bard::read "foo:bar")
;;; (symbol-package (bard::read "foo:bar"))
;;; (bard::read "#\\x")
;;; (bard::read "#\\space")
;;; (bard::print (bard::read "()") t)
;;; (bard::print (bard::read "(foo bard:bar Baz)") t)
;;; (bard::print (bard::read "{}") t)
;;; (bard::print (bard::read "{name \"Fred\" age 101}") t)
