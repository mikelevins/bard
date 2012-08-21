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

(cl:eval-when (:load-toplevel :compile-toplevel :execute)
  (cl:defparameter +bard-read-table+ (cl:copy-readtable))
  (cl:setf (cl:readtable-case +bard-read-table+) :preserve))

(cl:defun read (cl:&optional (stream cl:*standard-input*)(eof-error? cl:nil)(eof-value cl:nil))
  (cl:let ((cl:*readtable* +bard-read-table+))
    (cl:read stream eof-error? eof-value)))

;;; =====================================================================
;;; the evaluator
;;; =====================================================================

(cl:defmethod eval (x cl:&optional (env cl:nil)) (cl:eval x))

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

(cl:defmethod print ((obj fset:map) cl:&optional (s cl:*standard-output*))
  (cl:format s "{")
  (cl:let ((space-before? cl:nil))
    (fset:do-map (k v obj)
      (cl:when space-before? (cl:format s " "))
      (cl:setf space-before? cl:t)
      (print k s)
      (cl:format s " ")
      (print v s)))
  (cl:format s "}"))

;;; =====================================================================
;;; setup the toplevel
;;; =====================================================================

(cl:defparameter |bard.lang|::|nothing| nothing)
(cl:defparameter |bard.lang|::|true| true)
(cl:defparameter |bard.lang|::|false| false)

;;; =====================================================================
;;; run the repl
;;; =====================================================================

(cl:in-package #:cl-user)

(defun bard ()
  (block repl
    (format t "~%Bard 0.3-lisp~%")
    (loop
       (format t "~% bard> ")
       (let* ((*package* (find-package :|bard.lang|))
              (inp (bard::read)))
         (if (member inp '(:|q| :|quit| :q :quit))
             (progn
               (format t "~%Bard terminated~%")
               (return-from repl))
             (bard::print (bard::eval inp)))))))
