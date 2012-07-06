;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard read-eval-print
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(cl:in-package #:bard)

;;; =====================================================================
;;; the reader
;;; =====================================================================

;;; =====================================================================
;;; the evaluator
;;; =====================================================================

;;; =====================================================================
;;; the printer
;;; =====================================================================

(cl:defmethod print (obj cl:&optional (s cl:*standard-output*))
  (cl:format s "~S" obj))

(cl:defmethod print ((obj cl:null) cl:&optional (s cl:*standard-output*))
  (cl:format s "false"))

(cl:defmethod print ((obj (cl:eql true)) cl:&optional (s cl:*standard-output*))
  (cl:format s "true"))

(cl:defmethod print ((obj (cl:eql nothing)) cl:&optional (s cl:*standard-output*))
  (cl:format s "nothing"))

(cl:defmethod print ((obj fset:seq) cl:&optional (s cl:*standard-output*))
  (cl:format s "(")
  (cl:unless (cl:< (fset:size obj) 1)
    (print (fset:@ obj 0) s))
  (cl:unless (cl:< (fset:size obj) 2)
    (cl:loop for i from 1 below (fset:size obj)
             do (cl:progn
                  (cl:format s " ")
                  (print (fset:@ obj i)))))
  (cl:format s ")"))

;;; =====================================================================
;;; run the repl
;;; =====================================================================

(cl:in-package #:cl-user)

(defun bard ()
  )
