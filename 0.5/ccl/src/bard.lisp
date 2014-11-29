;;;; ***********************************************************************
;;;;
;;;; Name:          bard.lisp
;;;; Project:       bard 0.5
;;;; Purpose:       the bard programminglanguage
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

;;; ---------------------------------------------------------------------
;;; atoms: named constants
;;; ---------------------------------------------------------------------
;;; careful: this simple implementation provides no protection against
;;; redefining the named constants

(defmethod nothing? (x)
  (declare (ignore x))
  nil)

(defmethod nothing? ((x cl:null))
  (declare (ignore x))
  t)

(defmethod something? (x)
  (declare (ignore x))
  t)

(defmethod something? ((x cl:null))
  (declare (ignore x))
  nil)

(defclass end ()())
(defclass boolean ()())
(defclass true (boolean)())
(defclass false (boolean)())

(defmethod end? (x)
  (declare (ignore x))
  nil)

(defmethod end? ((x end))
  (declare (ignore x))
  t)

(defmethod true? (x)
  (declare (ignore x))
  t)

(defmethod true? ((x false))
  (declare (ignore x))
  nil)

(defmethod true? ((x cl:null))
  (declare (ignore x))
  nil)

(defmethod false? (x)
  (declare (ignore x))
  nil)

(defmethod false? ((x false))
  (declare (ignore x))
  t)

(defmethod false? ((x cl:null))
  (declare (ignore x))
  t)

(defparameter |nothing| nil)
(defparameter |end| (make-instance 'end))
(defparameter |true| (make-instance 'true))
(defparameter |false| (make-instance 'false))

;;; ---------------------------------------------------------------------
;;; lists
;;; ---------------------------------------------------------------------

(defun |list| (&rest items)
  items)

;;; ---------------------------------------------------------------------
;;; objects
;;; ---------------------------------------------------------------------

(defclass object ()
  ((slots :accessor slots :initarg :slots :initform nil)))

(defmethod print-object ((obj object)(str stream))
  (let ((slots (slots obj)))
    (princ "{" str)
    (when slots
      (let ((hd (first slots))
            (tl (rest slots)))
        (princ (car hd) str)
        (princ " " str)
        (princ (cdr hd) str)
        (when tl
          (loop for pair in tl
             do (progn
                  (princ " " str)
                  (princ (car pair) str)
                  (princ " " str)
                  (princ (cdr pair) str))))))
    (princ "}" str)))

(defun |object| (&rest slots)
  (let ((pairs (loop for tail on slots by #'cddr
                    collect (cons (car tail)(cadr tail)))))
    (make-instance 'object :slots pairs)))

;;; ---------------------------------------------------------------------
;;; set up the read table
;;; ---------------------------------------------------------------------

(defparameter *cl-readtable* com.informatimago.common-lisp.lisp-reader.reader:*readtable*)

(defparameter *bard-readtable*
  (let ((rtable (com.informatimago.common-lisp.lisp-reader.reader:copy-readtable *cl-readtable*)))
    (setf (com.informatimago.common-lisp.lisp-reader.reader:readtable-case rtable) :preserve)
    ;; list literals
    (com.informatimago.common-lisp.lisp-reader.reader:set-syntax-from-char #\] #\))
    (com.informatimago.common-lisp.lisp-reader.reader:set-macro-character
     #\[ (lambda (stream char)
           (declare (ignore char))
           (let ((items (com.informatimago.common-lisp.lisp-reader.reader:read-delimited-list #\] stream)))
             (cons '|list| items)))
     nil rtable)
    ;; object literals
    (com.informatimago.common-lisp.lisp-reader.reader:set-syntax-from-char #\} #\))
    (com.informatimago.common-lisp.lisp-reader.reader:set-macro-character
     #\{ (lambda (stream char)
           (declare (ignore char))
           (let ((items (com.informatimago.common-lisp.lisp-reader.reader:read-delimited-list #\} stream)))
             (cons '|object| items)))
     nil rtable)
    rtable))

(defmethod bard-read ((s stream))
  (let ((com.informatimago.common-lisp.lisp-reader.reader:*readtable* *bard-readtable*))
    (com.informatimago.common-lisp.lisp-reader.reader:read s nil |end| nil)))

(defmethod read-string ((s string))
  (with-input-from-string (in s)
    (bard-read in)))

(defmethod read-all-from-string ((s string))
  (with-input-from-string (in s)
    (loop for line = (bard-read in)
       until (eq line |end|)
       collect line)))

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(defmacro define-function (name lambda-list &rest body)
  `(defmethod ,name ,lambda-list ,@body))

(defmacro |define| (name &rest args)
  (case name
    ((|function|) (let* ((prototype (car args))
                         (fname (car prototype))
                         (params (cdr prototype))
                         (body (cdr args)))
                    `(define-function ,fname ,params ,@body)))
    (t `(defparameter ,name ,@args))))

;;; ---------------------------------------------------------------------
;;; repl
;;; ---------------------------------------------------------------------

(defun eval (expr)
  (cl:eval expr))

(defun bard-repl ()
  (block repl
    (loop
       (progn
         (format t "~%bard> ")
         (let* ((line (read-line))
                (input (read-string line))
                (output (eval input)))
           (format t "~A~%" output))))))

