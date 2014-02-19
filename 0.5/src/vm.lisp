;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard 0.5 virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)


;;; ==============================

(defstruct ret-addr method pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op) 
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))

(defun machine (f)
  "Run the abstract machine on the code for f."
  (let* ((code (code f))
         (pc 0)
         (env nil)
         (stack nil)
         (n-args 0)
         (instr nil))
    (loop
       (setf instr (elt code pc))
       (incf pc)
       (case (opcode instr)
         
         ;; Variable/stack manipulation instructions:
         (LVAR   (push (elt (elt env (arg1 instr)) (arg2 instr))
                       stack))
         (LSET   (setf (elt (elt env (arg1 instr)) (arg2 instr))
                       (top stack)))
         (GVAR   (push (get (arg1 instr) 'global-val) stack))
         (GSET   (setf (get (arg1 instr) 'global-val) (top stack)))
         (POP    (pop stack))
         (CONST  (push (arg1 instr) stack))
         
         ;; Branching instructions:
         (JUMP   (setf pc (arg1 instr)))
         (FJUMP  (if (false? (pop stack)) (setf pc (arg1 instr))))
         (TJUMP  (if (true? (pop stack)) (setf pc (arg1 instr))))
         
         ;; Function call/return instructions:
         (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                      :method f :env env)
                       stack))
         (RETURN ;; return value is top of stack; ret-addr is second
           (setf f (ret-addr-method (second stack))
                 code (code f)
                 env (ret-addr-env (second stack))
                 pc (ret-addr-pc (second stack)))
           ;; Get rid of the ret-addr, but keep the value
           (setf stack (cons (first stack) (rest2 stack))))
         (CALLJ  (pop env)                 ; discard the top frame
                 (setf f  (pop stack)
                       code (code f)
                       env (env f)
                       pc 0
                       n-args (arg1 instr)))
         (ARGS   (assert (= n-args (arg1 instr)) ()
                         "Wrong number of arguments:~
                         ~d expected, ~d supplied"
                         (arg1 instr) n-args)
                 (push (make-array (arg1 instr)) env)
                 (loop for i from (- n-args 1) downto 0 do
                      (setf (elt (first env) i) (pop stack))))
         (ARGS.  (assert (>= n-args (arg1 instr)) ()
                         "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
                         (arg1 instr) n-args)
                 (push (make-array (+ 1 (arg1 instr)) :initial-element nil) env)
                 (loop repeat (- n-args (arg1 instr)) do
                      (push (pop stack) (elt (first env) (arg1 instr))))
                 (loop for i from (- (arg1 instr) 1) downto 0 do
                      (setf (elt (first env) i) (pop stack))))
         (METHOD     (push (make-instance 'method :code (code (arg1 instr))
                                :env env) stack))
         (PROCEDURE     (push (make-instance 'procedure :code (code (arg1 instr))
                                             :env env) stack))
         (PRIM   (push (apply (arg1 instr)
                              (loop with args = nil repeat n-args
                                 do (push (pop stack) args)
                                 finally (return args)))
                       stack))
         
         ;; Continuation instructions:
         (SET-CC (setf stack (top stack)))
         (CC     (push (make-instance 'method
                        :env (list (vector stack))
                        :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                (LVAR 0 0) (RETURN)))
                       stack))
         
         ;; Nullary operations:
         ((BARD-READ NEWLINE) ; *** fix, gat, 11/9/92
          (push (funcall (opcode instr)) stack))
         
         ;; Unary operations:
         ((CAR CDR CADR END? NOT COMPILER DISPLAY WRITE RANDOM) 
          (push (funcall (opcode instr) (pop stack)) stack))
         
         ;; Binary operations:
         ((+ - * / < > <= >= /= = CONS NAME! EQ EQUAL EQL)
          (setf stack (cons (funcall (opcode instr) (second stack)
                                     (first stack))
                            (rest2 stack))))
         
         ;; Constants:
         ((T NIL -1 0 1 2)
          (push (opcode instr) stack))
         
         ;; Base-type constructors:

         ((MKBITS MKLIST MKRECORD MKVALUES)
          (setf stack
                (cons (funcall (opcode instr)(first stack))
                      (rest stack))))

         ;; Other:
         ((HALT) (RETURN (top stack)))
         (otherwise (error "Unknown opcode: ~a" instr))))))
