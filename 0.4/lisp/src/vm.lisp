;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard vm
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defstruct ret-addr fn pc env)

(defun top (stack) (first stack))

(defun vm (f)
  (let* ((code (mfn-code f))
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
         (LREF   (push (elt (elt env (arg1 instr)) (arg2 instr))
                       stack))
         (LSET   (setf (elt (elt env (arg1 instr)) (arg2 instr))
                       (top stack)))
         (GREF   (push (get (arg1 instr) 'global-val) stack))
         (GSET   (setf (get (arg1 instr) 'global-val) (top stack)))
         (POP    (pop stack))
         (CONST  (push (arg1 instr) stack))
         
         ;; Branching instructions:
         (GO   (setf pc (arg1 instr)))
         (FGO  (if (false? (pop stack)) (setf pc (arg1 instr))))
         (TGO  (if (true? (pop stack)) (setf pc (arg1 instr))))
         
         ;; Function call/return instructions:
         (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                      :fn f :env env)
                       stack))
         (RETURN ;; return value is top of stack; ret-addr is second
           (setf f (ret-addr-fn (second stack))
                 code (mfn-code f)
                 env (ret-addr-env (second stack))
                 pc (ret-addr-pc (second stack)))
           ;; Get rid of the ret-addr, but keep the value
           (setf stack (cons (first stack) (rest2 stack))))
         (CALLJ  (pop env)                 ; discard the top frame
                 (setf f  (pop stack)
                       code (mfn-code f)
                       env (mfn-env f)
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
                 (push (make-array (+ 1 (arg1 instr))) env)
                 (loop repeat (- n-args (arg1 instr)) do
                      (push (pop stack) (elt (first env) (arg1 instr))))
                 (loop for i from (- (arg1 instr) 1) downto 0 do
                      (setf (elt (first env) i) (pop stack))))
         (FN     (push (make-mfn :code (mfn-code (arg1 instr))
                                 :env env) stack))
         (PRIM   (push (apply (arg1 instr)
                              (loop with args = nil repeat n-args
                                 do (push (pop stack) args)
                                 finally (return args)))
                       stack))
         
         ;; Continuation instructions:
         (SET-CC (setf stack (top stack)))
         (CC     (push (make-mfn
                        :env (list (vector stack))
                        :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                (LVAR 0 0) (RETURN)))
                       stack))
         
         ;; Nullary operations:
         ((BARD-READ NEWLINE) ; *** fix, gat, 11/9/92
          (push (funcall (opcode instr)) stack))
         
         ;; Unary operations:
         ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM PRIMFIB) 
          (push (funcall (opcode instr) (pop stack)) stack))
         
         ;; Binary operations:
         ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
          (setf stack (cons (funcall (opcode instr) (second stack)
                                     (first stack))
                            (rest2 stack))))
         
         ;; Ternary operations:
         (LIST3
          (setf stack (cons (funcall (opcode instr) (third stack)
                                     (second stack) (first stack))
                            (rest3 stack))))
         
         ;; Constants:
         (TRUE (push (true) stack))
         (FALSE (push (false) stack))
         (UNDEFINED (push (undefined) stack))
         (NOTHING (push (nothing) stack))
         
         ;; Other:
         ((HALT) (RETURN (top stack)))
         (otherwise (error "Unknown opcode: ~a" instr))))))
