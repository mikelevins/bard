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
  ((vm :initarg :vm :reader vm)))

(define-condition undefined-vm-global (condition)
  ((vm :initarg vm :reader vm)
   (varname :initarg :varname :reader varname)))

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
(defbytecode VPRINTLN 1) 
(defbytecode VNIL 2) 
(defbytecode VT 3) 
(defbytecode LREF 4) 
(defbytecode LSET 5)
(defbytecode GREF 6)
(defbytecode GSET 7)
(defbytecode VPOP 8)
(defbytecode CONST 9)
(defbytecode JUMP 10)
(defbytecode FJUMP 11)
(defbytecode TJUMP 12)
(defbytecode VSAVE 13)
(defbytecode VRET 14)
(defbytecode CALL 15)
(defbytecode ARGS 16)
(defbytecode ARGS& 17)
(defbytecode FN 18)
(defbytecode PRIM 19)
(defbytecode SETCC 20)
(defbytecode CC 21)
(defbytecode -ONE 22)
(defbytecode ZERO 23)
(defbytecode ONE 24)
(defbytecode TWO 25)
(defbytecode INC1 26)
(defbytecode DEC1 27)
(defbytecode VEQ 28)
(defbytecode VEQL 29)
(defbytecode ADD 30)
(defbytecode SUB 31)

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
(defmethod arg1 ((vm vm))(first (instr-args (vm-instr vm))))
(defmethod arg2 ((vm vm))(first (instr-args (vm-instr vm))))
(defmethod arg3 ((vm vm))(first (instr-args (vm-instr vm))))
(defmethod args ((vm vm))(instr-args (vm-instr vm)))

(defclass vm ()
  ((code :initform nil :accessor vm-code)
   (pc :accessor vm-pc :initform 0)
   (globals :accessor vm-globals :initform (make-hash-table))
   (env :accessor vm-env :initform nil)
   (stack :accessor vm-stack :initform nil)
   (nargs :accessor vm-nargs :initform 0)
   (instr :accessor vm-instr :initform nil)))


(defmethod vm-show-globals ((vm vm) &key (stream *standard-output*))
  (let ((globals (vm-globals vm)))
    (format stream "~%globals:")
    (loop for k being the hash-keys in globals using (hash-value v)
          do (format stream "~%  ~S ~S" k v))))

(defmethod vmreset ((vm vm))
  (setf (vm-pc vm) 0)
  (setf (vm-globals vm) (make-hash-table))
  (setf (vm-env vm) nil)
  (setf (vm-stack vm) nil)
  (setf (vm-nargs vm) 0)
  (setf (vm-instr vm) nil))

(defmethod vm-gref ((vm vm)(var symbol))
  (let ((found (gethash var (vm-globals vm) :global-absent)))
    (if (eq found :global-absent)
        (signal 'undefined-vm-global :varname var :vm vm)
        found)))

#+repl (vm-gref $vm 'x)

(defmethod vm-gset! ((vm vm)(var symbol) val)
  (setf (gethash var (vm-globals vm))
        val))

(defmethod vmload ((vm vm)(code vector))
  (setf (vm-code vm) code)
  (vmreset vm))

(defmethod stepvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (progn (setf instr (elt code pc))
           (incf pc)
           (let ((bc (instr-bytecode instr)))
             ;; machine control
             (cond ((eql HALT bc)(signal 'exitvm)))
             (cond ((eql VPOP bc)(pop stack)))
             (cond ((eql VPRINTLN bc)(format t "~%~S" (pop stack))))
             ;; globals
             (cond ((eql GREF bc)(push (vm-gref vm (pop stack)) stack)))
             (cond ((eql GSET bc)(let ((args nil))
                                   (push (pop stack) args)
                                   (push (pop stack) args)
                                   (apply 'vm-gset! vm args))))
             ;; branching
             (cond ((eql JUMP bc)(setf pc (arg1 vm))))
             (cond ((eql FJUMP bc)(when (null (pop stack))(setf pc (arg1 vm)))))
             (cond ((eql TJUMP bc)(when (pop stack)(setf pc (arg1 vm)))))
             ;; constant ops
             (cond ((eql VNIL bc)(push nil stack)))             
             (cond ((eql VT bc)(push t stack)))             
             (cond ((eql -ONE bc)(push -1 stack)))             
             (cond ((eql ZERO bc)(push 0 stack)))             
             (cond ((eql ONE bc)(push 1 stack)))             
             (cond ((eql TWO bc)(push 2 stack)))             
             ;; unary ops
             (cond ((eql CONST bc)(push (arg1 vm) stack)))
             (cond ((eql INC1 bc)(push (1+ (pop stack)) stack)))
             (cond ((eql DEC1 bc)(push (1- (pop stack)) stack)))
             ;; binary ops
             (cond ((eql VEQ bc)(let ((args nil))
                                  (push (pop stack) args)
                                  (push (pop stack) args)
                                  (push (apply 'eq args)
                                        stack))))
             (cond ((eql VEQL bc)(let ((args nil))
                                   (push (pop stack) args)
                                   (push (pop stack) args)
                                   (push (apply 'eql args)
                                         stack))))
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

(defmethod stepshowvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (if (< pc (length code))
        (progn (format t "~%~S" (elt code pc))
           (stepvm vm)
           (describe vm))
        (format t "~%Done."))))

(defmethod runvm ((vm vm))
  (with-slots (code pc env stack nargs instr) vm
    (handler-case (loop (stepvm vm))
      (exitvm (c) (top stack)))))

#+repl (defparameter $vm (make-instance 'vm))
#+repl (describe $vm)
#+repl (stepvm $vm)
#+repl (stepshowvm $vm)
#+repl (vm-show-globals $vm)
#+repl (runvm $vm)
#+repl (vmreset $vm)
#+repl (vmload $vm (vector (instr HALT)))
#+repl (vmload $vm (vector (instr CONST 5)(instr HALT)))
#+repl (vmload $vm (vector (instr -ONE)(instr HALT)))
#+repl (vmload $vm (vector (instr TWO)(instr HALT)))
#+repl (vmload $vm (vector (instr CONST 2)(instr CONST 3)(instr ADD)(instr HALT)))
#+repl (vmload $vm (vector (instr CONST 5)(instr CONST 2)(instr SUB)(instr HALT)))
#+repl (vmload $vm (vector (instr VNIL)(instr FJUMP 3)(instr CONST 101)(instr HALT)))
#+repl (vmload $vm (vector (instr CONST 'X)   ; 0
                           (instr CONST 10000)  ; 1
                           (instr GSET)       ; 2
                           (instr CONST 'X)   ; 3
                           (instr GREF)       ; 4
                           (instr ZERO)       ; 5
                           (instr VEQL)       ; 6
                           (instr TJUMP 18)   ; 7
                           (instr CONST 'X)   ; 8
                           (instr GREF)       ; 9                           
                           (instr VPRINTLN)   ; 10                           
                           (instr VPOP)       ; 11
                           (instr CONST 'X)   ; 12
                           (instr CONST 'X)   ; 13
                           (instr GREF)       ; 14
                           (instr DEC1)       ; 15
                           (instr GSET)       ; 16
                           (instr JUMP 3)     ; 17
                           (instr HALT)))

#+repl (defparameter $vm (make-instance 'vm))
#+repl (time (runvm $vm))
#+repl (time (loop for i from 10000 above 0 do (format t "~%~S" i)))
