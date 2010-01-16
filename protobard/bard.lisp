;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Bard.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       build the Bard application image
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(require "OBJC-SUPPORT")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((load-path (ccl::loading-file-source-file))
         (load-dir (make-pathname :directory (pathname-directory load-path)))) 
    (defun base-directory () load-dir)))

;;; ============================================================
;;; Package BARD
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage BARD 
    (:use common-lisp ccl)
    (:shadow = boolean character compile false first fourth intern keyword map number read rest second sequence symbol third true text)))

(in-package :bard)

;;; ============================================================
;;; BARD values
;;; ============================================================

;;; ------------------------------------------------------------
;;; singleton classes for unique Bard values
;;; ------------------------------------------------------------
;;; from Tim Bradshaw's example at:
;;; http://www.tfeb.org/programs/lisp/singleton-class.lisp
;;; copyright 2002 by TIm Bradshaw

(defclass singleton-class (standard-class)
  ((instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)
                                (superclass standard-class))
  ;; it's OK for a standard class to be a superclass of a singleton
  ;; class
  t)

(defmethod validate-superclass ((class singleton-class)
                                (superclass singleton-class))
  ;; it's OK for a singleton class to be a subclass of a singleton class
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass singleton-class))
  ;; but it is not OK for a standard class which is not a singleton class
  ;; to be a subclass of a singleton class
  nil)

(defmethod make-instance ((class singleton-class)
                          &key)
  (with-slots (instance) class
    (or instance
        (setf instance (call-next-method)))))

(defvar *singleton-classes* '())

(defmethod initialize-instance :after ((c singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  ;; This means you will get new singletons from now on.
  (loop for c in *singleton-classes*
        do (setf (slot-value c 'instance) nil)))


;;; ------------------------------------------------------------
;;; superclass for all Bard values
;;; ------------------------------------------------------------

(defclass bard-value ()())

;;; ============================================================
;;; base Bard types
;;; ============================================================

(defmethod = (x y)
  (cl:= x y))

;;; ------------------------------------------------------------
;;; Void
;;; ------------------------------------------------------------

(defclass void (bard-value)()(:metaclass singleton-class))

(defun void ()(make-instance 'void))

(defmethod print-object ((v void)(s stream))
  (format s "void"))

(defmethod void? (x)(declare (ignore x)) nil)
(defmethod void? ((x void))(declare (ignore x)) t)

(defmethod = ((x void) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y void))
  (declare (ignore x))
  nil)

(defmethod = ((x void) (y void))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Number
;;; ------------------------------------------------------------

(defclass number (bard-value)
  ((value :reader value :initarg :value :type 'cl:number)))

(defmethod initialize-instance :before ((n number) &key (value 0) &allow-other-keys)
  (assert (cl:numberp value)()
          "Invalid value for number: ~S" value))

(defmethod number ((n cl:number))
  (make-instance 'number :value n))

(defmethod print-object ((n number)(s stream))
  (format s "~d" (value n)))

(defmethod number? (x)(declare (ignore x)) nil)
(defmethod number? ((x number))(declare (ignore x)) t)

(defmethod = ((x number) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y number))
  (declare (ignore x))
  nil)

(defmethod = ((x number) (y number))
  (cl:= (value x)(value y)))

;;; ------------------------------------------------------------
;;; Character
;;; ------------------------------------------------------------

(defclass character (bard-value)
  ((value :reader value :initarg :value :type 'cl:character)))

(defmethod initialize-instance :before ((c character) &key (value #\null) &allow-other-keys)
  (assert (cl:characterp value)()
          "Invalid value for character: ~S" value))

(defmethod character ((c cl:character))
  (make-instance 'character :value c))

(defmethod print-object ((c character)(s stream))
  (format s "\\~a" (value c)))

(defmethod character? (x)(declare (ignore x)) nil)
(defmethod character? ((x character))(declare (ignore x)) t)

(defmethod = ((x character) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y character))
  (declare (ignore x))
  nil)

(defmethod = ((x character) (y character))
  (cl:char= (value x)(value y)))

;;; ------------------------------------------------------------
;;; Name
;;; ------------------------------------------------------------

(defclass name (bard-value)
  ((name :reader name :initarg :name :type 'cl:symbol)))
(defmethod name? (x)(declare (ignore x)) nil)
(defmethod name? ((x name))(declare (ignore x)) t)

;;; Keyword
;;; ------------------------------------------------------------

(defclass keyword (name)())

(defmethod initialize-instance :before ((k keyword) &key (name nil) &allow-other-keys)
  (assert (cl:symbolp name)()
          "Invalid value for keyword: ~S" name))

(defmethod keyword ((s cl:symbol))
  (make-instance 'keyword :name (cl:intern (symbol-name s) (find-package :bard))))

(defmethod keyword ((s cl:string))
  (make-instance 'keyword :name (cl:intern s (find-package :bard))))

(defmethod print-object ((k keyword)(s stream))
  (format s "~a:" (name k)))

(defmethod keyword? (x)(declare (ignore x)) nil)
(defmethod keyword? ((x keyword))(declare (ignore x)) t)

(defmethod = ((x keyword) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y keyword))
  (declare (ignore x))
  nil)

(defmethod = ((x keyword) (y keyword))
  (cl:eql (name x)(name y)))

;;; Symbol
;;; ------------------------------------------------------------

(defclass symbol (name)())

(defmethod initialize-instance :before ((s symbol) &key (name nil) &allow-other-keys)
  (assert (cl:symbolp name)()
          "Invalid value for symbol: ~S" name))

(defmethod symbol ((s cl:symbol))
  (make-instance 'symbol :name (cl:intern (symbol-name s) (find-package :bard))))

(defmethod symbol ((s cl:string))
  (make-instance 'symbol :name (cl:intern s (find-package :bard))))

(defmethod print-object ((sym symbol)(s stream))
  (format s "~a" (name sym)))

(defmethod symbol? (x)(declare (ignore x)) nil)
(defmethod symbol? ((x symbol))(declare (ignore x)) t)

(defmethod = ((x symbol) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y symbol))
  (declare (ignore x))
  nil)

(defmethod = ((x symbol) (y symbol))
  (cl:eql (name x)(name y)))

;;; ------------------------------------------------------------
;;; Booleans
;;; ------------------------------------------------------------

(defclass boolean (bard-value)())
(defmethod boolean? (x)(declare (ignore x)) nil)
(defmethod boolean? ((x boolean))(declare (ignore x)) t)

;;; True
;;; ------------------------------------------------------------

(defclass true (boolean)()(:metaclass singleton-class))

(defun true ()(make-instance 'true))

(defmethod print-object ((tr true)(s stream))
  (format s "true"))

(defmethod = ((x true) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y true))
  (declare (ignore x))
  nil)

(defmethod = ((x true) (y true))
  (declare (ignore x y))
  t)

;;; False
;;; ------------------------------------------------------------

(defclass false (boolean)()(:metaclass singleton-class))

(defun false ()(make-instance 'false))

(defmethod print-object ((f false)(s stream))
  (format s "false"))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)
(defmethod true? (x)(not (false? x)))

(defmethod = ((x false) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y false))
  (declare (ignore x))
  nil)

(defmethod = ((x false) (y false))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Sequences
;;; ------------------------------------------------------------

(defclass sequence (bard-value)())
(defmethod sequence? (x)(declare (ignore x)) nil)
(defmethod sequence? ((x sequence))(declare (ignore x)) t)

;;; cons-sequence
;;; ------------------------------------------------------------

(defclass cons-sequence (sequence)
  ((items :reader items :initarg :items)))

(defun sequence (&rest items)
  (make-instance 'cons-sequence :items items))

(defmethod print-object ((cs cons-sequence)(s stream))
  (format s "~S" (items cs)))

(defmethod = ((x cons-sequence) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cons-sequence))
  (declare (ignore x))
  nil)

(defmethod = ((x cons-sequence) (y cons-sequence))
  (every (lambda (i j)(= i j))
         (items x)
         (items y)))

;;; ------------------------------------------------------------
;;; Text
;;; ------------------------------------------------------------

(defclass text (sequence)())
(defmethod text? (x)(declare (ignore x)) nil)
(defmethod text? ((x text))(declare (ignore x)) t)

;;; unicode-text
;;; ------------------------------------------------------------

(defclass unicode-text (text)
  ((value :reader value :initarg :value)))

(defmethod text ((s string))
  (make-instance 'unicode-text :value s))

(defmethod print-object ((tx unicode-text)(s stream))
  (format s "\"~a\"" (value tx)))

(defmethod = ((x unicode-text) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y unicode-text))
  (declare (ignore x))
  nil)

(defmethod = ((x unicode-text) (y unicode-text))
  (cl:string= (value x)(value y)))

;;; ============================================================
;;; BARD reader
;;; ============================================================
