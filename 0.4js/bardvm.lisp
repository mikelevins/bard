;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.lisp
;;;; Project:       bardvm
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bardvm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *js-string-delimiter* #\"))

(defun compile-kernel (in-path out-path)
  (with-open-file (out out-path :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (format out (ps:ps-compile-file in-path))))

;;; (defparameter $src "/Users/mikel/Workshop/programming/bard/0.4js/kernel.parenscript")
;;; (defparameter $obj "/Users/mikel/Workshop/programming/bard/0.4js/kernel.js")
;;; (compile-kernel $src $obj)

