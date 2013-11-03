;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter *primitives* (make-hash-table))

(defun defprim (name argcount prim)
  (let ((prim-array (or (gethash name *primitives* nil)
                        (make-array 16 :initial-element nil))))
    (setf (aref prim-array argcount) prim)
    (setf (gethash name *primitives*) prim-array)
    name))

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

(defmethod primitive? ((x symbol) env (argcount integer))
  (and (not (in-environment? x env))
       (let ((prim-array (gethash x *primitives*)))
         (if prim-array
             (let ((p (aref prim-array argcount)))
               (and (typep p '<primitive>)
                    (eql x (prim-name p))
                    (= argcount (prim-n-args p))
                    p))))))

