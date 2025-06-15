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

(defmethod print-object ((instr instr) stream)
  (format stream "[~S ~{~S ~}]"
          (bytecode->opcode (instr-bytecode instr))
          (instr-args instr)))

(defstruct (instr)
  (bytecode 0)
  (args nil))

(defun instr (bc &rest args)
  (make-instr :bytecode bc :args args))

#+repl (instr HALT)

(defmacro defbytecode (name bytecode)
  `(progn (defparameter ,name ,bytecode)
          (defmethod bytecode->opcode ((b (eql ,bytecode))) ',name)))

(defbytecode HALT 0) ; (bytecode->opcode HALT)
(defbytecode VNIL 1) 
(defbytecode VT 2) 
(defbytecode LVAR 3) 
(defbytecode LSET 4)
(defbytecode GVAR 5)
(defbytecode GSET 6)
(defbytecode VPOP 7)
(defbytecode CONST 8)
(defbytecode JUMP 9)
(defbytecode FJUMP 10)
(defbytecode TJUMP 11)
(defbytecode VSAVE 12)
(defbytecode VRET 13)
(defbytecode CALL 14)
(defbytecode ARGS 15)
(defbytecode ARGS& 16)
(defbytecode FN 17)
(defbytecode PRIM 18)
(defbytecode SETCC 19)
(defbytecode CC 20)
(defbytecode -ONE 21)
(defbytecode ZERO 22)
(defbytecode ONE 23)
(defbytecode TWO 22)
(defbytecode ADD 23)
(defbytecode SUB 24)

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

(defmethod vmreset ((vm vm))
  (setf (vm-pc vm) 0))

(defmethod vmload ((vm vm)(code vector))
  (setf (vm-code vm) code)
  (vmreset vm))

(defmethod stepvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (progn (setf instr (elt code pc))
           (incf pc)
           (let ((bc (instr-bytecode instr)))
             ;; nullary ops
             (cond ((eql HALT bc)(signal 'exitvm)))
             ;; constant ops
             (cond ((eql VNIL bc)(push nil stack)))             
             (cond ((eql VT bc)(push t stack)))             
             (cond ((eql -ONE bc)(push -1 stack)))             
             (cond ((eql ZERO bc)(push 0 stack)))             
             (cond ((eql ONE bc)(push 1 stack)))             
             (cond ((eql TWO bc)(push 2 stack)))             
             ;; unary ops
             (cond ((eql CONST bc)(push (first (instr-args instr))
                                        stack)))
             ;; binary ops
             (cond ((eql ADD bc)(let ((args nil))
                                  (push (pop stack) args)
                                  (push (pop stack) args)
                                  (push (apply '+ args)
                                        stack))))
             (cond ((eql SUB bc)(let ((args nil))
                                  (push (pop stack) args)
                                  (push (pop stack) args)
                                  (push (apply '- args)
                                        stack))))))))

(defmethod runvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (handler-case (loop (stepvm vm))
      (exitvm (c) (top stack)))))

#+repl (defparameter $vm (make-instance 'vm))
#+repl (describe $vm)
#+repl (runvm $vm)
#+repl (stepvm $vm)
#+repl (vmreset $vm)
#+repl (vmload $vm (vector (instr HALT)))
#+repl (vmload $vm (vector (instr CONST 5)(instr HALT)))
#+repl (vmload $vm (vector (instr -ONE)(instr HALT)))
#+repl (vmload $vm (vector (instr TWO)(instr HALT)))
#+repl (vmload $vm (vector (instr CONST 2)(instr CONST 3)(instr ADD)(instr HALT)))
#+repl (vmload $vm (vector (instr CONST 5)(instr CONST 2)(instr SUB)(instr HALT)))
