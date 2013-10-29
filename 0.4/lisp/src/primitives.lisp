;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard primitives
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter *primitives* (make-hash-table))

(defun defprim (name prim)
  (setf (gethash name *primitives*) prim)
  name)

(defclass <primitive> ()
  ((name :accessor prim-name :initform nil :initarg :name)
   (n-args :accessor prim-n-args :initform 0 :initarg :n-args)
   (opcode :accessor prim-opcode :initform nil :initarg :opcode)
   (always? :accessor prim-always? :initform nil :initarg :always)
   (side-effects? :accessor prim-side-effects? :initform nil :initarg :side-effects)))

(defun make-prim (&key name n-args opcode always side-effects)
  (make-instance '<primitive>
                 :name name
                 :n-args n-args
                 :opcode opcode
                 :always always
                 :side-effects side-effects))

(defmethod primitive? (x env argcount)
  (declare (ignore x env argcount))
  nil)

(defmethod primitive? ((x symbol) (env <environment>) (argcount integer))
  (and (not (in-environment? x env))
       (let ((p (gethash x *primitives*)))
         (and (typep p '<primitive>)
              (eql x (prim-name p))
              (= argcount (prim-n-args p))
              p))))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::+ 
    (make-prim :name 'bard-symbols::+
               :n-args 2
               :opcode 'cl:+
               :always t
               :side-effects nil))


