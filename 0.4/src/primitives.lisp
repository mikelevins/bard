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

(defprim 'bard-symbols::|newline| 
    (make-prim :name 'bard-symbols::|newline|
               :n-args 0
               :opcode 'bard::newline
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|display| 
    (make-prim :name 'bard-symbols::|display|
               :n-args 1
               :opcode 'bard::display
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|write| 
    (make-prim :name 'bard-symbols::|write|
               :n-args 1
               :opcode 'cl:write
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|compiler| 
    (make-prim :name 'bard-symbols::|compiler|
               :n-args 1
               :opcode 'bard::compiler
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|read| 
    (make-prim :name 'bard-symbols::|read|
               :n-args 0
               :opcode 'bard::bard-read
               :always nil
               :side-effects t))


