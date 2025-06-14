;;;; ***********************************************************************
;;;;
;;;; Name:          vm.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; conditions

(define-condition exitvm (condition)
  ((vm :initarg vm :reader vm)))

;;; Instructions

(defstruct (instr (:type vector))
  (bytecode 0)
  (nargs 0)
  (args nil))

(defun instr (bc nargs &rest args)
  (make-instr :bytecode bc :nargs nargs :args args))

(defmacro defbytecode (name bytecode)
  `(progn (defparameter ,name ,bytecode)
          (defmethod bytecode->opcode ((b (eql ,bytecode))) ,name)))

(defbytecode HALT 0) ; (bytecode->opcode HALT)
(defbytecode LVAR 1) ; (bytecode->opcode LVAR)
(defbytecode LSET 2)
(defbytecode GVAR 3)
(defbytecode GSET 4)
(defbytecode POPV 5)
(defbytecode CONST 6)
(defbytecode JUMP 7)
(defbytecode FJUMP 8)
(defbytecode TJUMP 9)
(defbytecode SAVEV 10)
(defbytecode RETV 11)
(defbytecode CALL 12)
(defbytecode ARGS 13)
(defbytecode ARGS& 14)
(defbytecode FN 15)
(defbytecode PRIM 16)
(defbytecode SETCC 17)
(defbytecode CC 18)


;;; functions

(defstruct (fn)
  code
  (env nil)
  (name nil)
  (args nil))

(defstruct (cc)
  code
  (env nil)
  (args nil))

;;; vm

(defun top (s)(first s))

(defclass vm ()
  ((code :initform nil :accessor vm-code)
   (pc :accessor vm-pc :initform 0)
   (env :accessor vm-env :initform nil)
   (stack :accessor vm-stack :initform nil)
   (nargs :accessor vm-nargs :initform 0)
   (instr :accessor vm-instr :initform nil)))

(defmethod stepvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (progn (setf instr (elt code pc))
           (incf pc)
           (let ((bc (instr-bytecode instr)))
             (cond ((eql HALT bc)(signal 'exitvm)))
             (cond ((eql CONST bc)(push (first (instr-args instr))
                                        stack)))))))

(defmethod runvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (handler-case (loop (stepvm vm))
      (exitvm (c) (top stack)))))

#+repl (defparameter $vm (make-instance 'vm))
#+repl (describe $vm)
#+repl (runvm $vm)
#+repl (stepvm $vm)
#+repl (setf (vm-code $vm) (vector (instr HALT 0)))
#+repl (setf (vm-code $vm) (vector (instr CONST 1 5)(instr HALT 0)))
