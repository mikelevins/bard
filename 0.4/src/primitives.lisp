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

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::+ 2
    (make-prim :name 'bard-symbols::+
               :n-args 2
               :opcode 'cl:+
               :always t
               :side-effects nil))

(defprim 'bard-symbols::* 2
    (make-prim :name 'bard-symbols::*
               :n-args 2
               :opcode 'cl:*
               :always t
               :side-effects nil))

(defprim 'bard-symbols::- 2
    (make-prim :name 'bard-symbols::-
               :n-args 2
               :opcode 'cl:-
               :always t
               :side-effects nil))

(defprim 'bard-symbols::/ 2
    (make-prim :name 'bard-symbols::/
               :n-args 2
               :opcode 'cl:/
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|newline| 0
    (make-prim :name 'bard-symbols::|newline|
               :n-args 0
               :opcode 'bard::newline
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|display| 1
    (make-prim :name 'bard-symbols::|display|
               :n-args 1
               :opcode 'bard::display
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|write| 1
    (make-prim :name 'bard-symbols::|write|
               :n-args 1
               :opcode 'cl:write
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|compiler| 1
    (make-prim :name 'bard-symbols::|compiler|
               :n-args 1
               :opcode 'bard::compiler
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|read| 0
    (make-prim :name 'bard-symbols::|read|
               :n-args 0
               :opcode 'bard::bard-read
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|pair| 2
    (make-prim :name 'bard-symbols::|pair|
               :n-args 2
               :opcode 'cl:cons
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|pair.left| 1
    (make-prim :name 'bard-symbols::|pair.left|
               :n-args 1
               :opcode 'bard::pair.left
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|pair.right| 1
    (make-prim :name 'bard-symbols::|pair.right|
               :n-args 1
               :opcode 'bard::pair.right
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 0
    (make-prim :name 'bard-symbols::|list|
               :n-args 0
               :opcode 'bard::list0
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 1
    (make-prim :name 'bard-symbols::|list|
               :n-args 1
               :opcode 'bard::list1
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 2
    (make-prim :name 'bard-symbols::|list|
               :n-args 2
               :opcode 'bard::list2
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 3
    (make-prim :name 'bard-symbols::|list|
               :n-args 3
               :opcode 'bard::list3
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 4
    (make-prim :name 'bard-symbols::|list|
               :n-args 4
               :opcode 'bard::list4
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 5
    (make-prim :name 'bard-symbols::|list|
               :n-args 5
               :opcode 'bard::list5
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 6
    (make-prim :name 'bard-symbols::|list|
               :n-args 6
               :opcode 'bard::list6
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 7
    (make-prim :name 'bard-symbols::|list|
               :n-args 7
               :opcode 'bard::list7
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 8
    (make-prim :name 'bard-symbols::|list|
               :n-args 8
               :opcode 'bard::list8
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 9
    (make-prim :name 'bard-symbols::|list|
               :n-args 9
               :opcode 'bard::list9
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|list| 10
    (make-prim :name 'bard-symbols::|list|
               :n-args 10
               :opcode 'bard::list10
               :always t
               :side-effects nil))




