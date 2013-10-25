;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          classes.lisp
;;;; Project:       Bard
;;;; Purpose:       definitions of classes used in the compiler and vm
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <agent>
;;; ---------------------------------------------------------------------

(defclass <agent> ()
  ((compiler :accessor compiler :initform (make-standard-compiler) :initarg :compiler)
   (vm :accessor vm :initform (make-standard-vm) :initarg :vm)
   (image :accessor image :initform nil :initarg :image)
   (messages :accessor messages :initform nil :initarg :messages)))

(defmethod initialize-instance :after ((agent <agent>) &rest initargs &key &allow-other-keys)
  (setf (slot-value (compiler agent) 'agent) agent))

;;; ---------------------------------------------------------------------
;;; <compiler>
;;; ---------------------------------------------------------------------

(defclass <compiler> ()
  ((version :reader version :initform *bard-compiler-version*)
   (globals :reader globals :initform (make-standard-globals))
   (modules :reader modules :initform (make-hash-table))
   (agent :reader agent :initform nil)
   (primitives :reader primitives :initform nil)
   (macros :reader macros :initform nil)))

(defmethod initialize-instance :after ((comp <compiler>) &rest initargs &key &allow-other-keys)
  (let ((mods (make-standard-modules comp)))
    (setf (slot-value comp 'modules) mods)))

;;; ---------------------------------------------------------------------
;;; <environment>
;;; ---------------------------------------------------------------------

(defclass <environment> ()
  ((bindings :accessor bindings :initform nil :initarg :bindings)))

;;; ---------------------------------------------------------------------
;;; <globals>
;;; ---------------------------------------------------------------------

(defclass <globals> ()
  ((next-id :accessor next-id :initform 0)
   (variables :accessor variables
             :initform (make-array 256 :adjustable t :initial-element (undefined)))))

;;; ---------------------------------------------------------------------
;;; <mvar>
;;; ---------------------------------------------------------------------

(defclass <mvar> ()
  ((name :accessor name :initform nil)
   (id :accessor id :initform nil)
   (mutable? :accessor mutable? :initform nil)
   (exported? :accessor exported? :initform nil)
   (import-from :accessor import-from :initform nil)
   (import-name :accessor import-name :initform nil)))


;;; ---------------------------------------------------------------------
;;; <module>
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((globals :accessor globals :initform nil :initarg :globals)
   (variables-by-name :accessor variables-by-name :initform (make-hash-table))
   (variables-by-id :accessor variables-by-id :initform (make-hash-table))))
