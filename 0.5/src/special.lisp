;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.lisp
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; abort
;;; and

(defmacro |and| (&rest forms)
  (if (null forms)
      |true|
      (let ((test (first forms))
            (more (rest forms)))
        (if (null more)
            test
            `(if (true? ,test)
                 (|and| ,@more)
                 |nothing|)))))

;;; begin

(defmacro |begin| (&rest forms)
  `(cl:progn ,@forms))

;;; case
;;; catch
;;; cond
;;; define
;;;   class
;;;   condition
;;;   constant
;;;   macro
;;;   method
;;;   record
;;;   setter
;;;   tuple
;;;   union
;;;   variable
;;; do
;;; dolist
;;; dotimes
;;; ensure
;;; eval
;;; function
;;; generate
;;; handler-bind
;;; handler-case
;;; if

(defmacro |if| (test then else)
  `(if (true? ,test)
       ,then
       ,else))

;;; invoke-restart
;;; let
;;; loop

(defmacro |loop| (loop-name bindings &rest body)
  `(recur:recur ,loop-name ,bindings ,@body))

;;; match
;;; method
;;; next-method
;;; or

(defmacro |or| (&rest forms)
  (if (null forms)
      |nothing|
      (let ((test (first forms))
            (more (rest forms)))
        (if (null more)
            test
            `(let ((val ,test))
               (if (true? val)
                   val
                   (|or| ,@more)))))))

;;; receive
;;; restart-bind
;;; restart-case
;;; set!
;;; send
;;; setter
;;; signal
;;; the ; advises the reader what type to use for a read object
;;; throw
;;; unless

(defmacro |unless| (test &rest forms)
  `(if (false? ,test)
       (progn ,@forms)
       |nothing|))

;;; values
;;; when

(defmacro |when| (test &rest forms)
  `(if (true? ,test)
       (progn ,@forms)
       |nothing|))

;;; with-exit
;;; with-open

