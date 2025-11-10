;;;; ***********************************************************************
;;;;
;;;; Name:          auxfns.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       auxiliary functions
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defclass vm ()
  ((code :accessor code :initform nil :initarg :code)
   (pc :accessor pc :initform 0 :initarg :pc)
   (env :accessor env :initform nil :initarg :env)
   (stack :accessor stack :initform nil :initarg :stack)
   (n-args :accessor n-args :initform 0 :initarg :n-args)
   (instr :accessor instr :initform nil :initarg :instr)
   (halted? :accessor halted? :initform t :initarg :halted)))

(defmethod runvm ((vm vm)(fn fn))
  (with-slots (code pc env stack n-args instr halted?) vm
    (setf code (fn-code fn)
          pc 0
          env nil
          stack nil
          n-args 0
          instr nil
          halted? nil)
    (loop
      (if halted?
          ;; stop the vm
          (return (top stack))
          ;; step the vm
          (progn
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
              (FJUMP  (if (null (pop stack)) (setf pc (arg1 instr))))
              (TJUMP  (if (pop stack) (setf pc (arg1 instr))))

              ;; Function call/return instructions:
              (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                           :fn fn :env env)
                            stack))
              (RETURN ;; return value is top of stack; ret-addr is second
                (setf fn (ret-addr-fn (second stack))
                      code (fn-code fn)
                      env (ret-addr-env (second stack))
                      pc (ret-addr-pc (second stack)))
                ;; Get rid of the ret-addr, but keep the value
                (setf stack (cons (first stack) (drop 2 stack))))
              (CALLJ  (pop env)                 ; discard the top frame
               (setf fn  (pop stack)
                     code (fn-code fn)
                     env (fn-env fn)
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
              (FN     (push (make-fn :code (fn-code (arg1 instr))
                                     :env env) stack))
              (PRIM   (push (apply (arg1 instr)
                                   (loop with args = nil repeat n-args
                                         do (push (pop stack) args)
                                         finally (return args)))
                            stack))

              ;; Continuation instructions:
              (SET-CC (setf stack (top stack)))
              (CC     (push (make-fn
                             :env (list (vector stack))
                             :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                     (LVAR 0 0) (RETURN)))
                            stack))

              ;; Nullary operations:
              ((BARD-READ NEWLINE) ; *** fix, gat, 11/9/92
               (push (funcall (opcode instr)) stack))

              ;; Unary operations:
              ((CAR CDR CADR EOF-OBJECT? NOT LIST1 COMPILER DISPLAY WRITE RANDOM)
               (push (funcall (opcode instr) (pop stack)) stack))

              ;; Binary operations:
              ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
               (setf stack (cons (funcall (opcode instr) (second stack)
                                          (first stack))
                                 (drop 2 stack))))

              ;; Ternary operations:
              (LIST3
               (setf stack (cons (funcall (opcode instr) (third stack)
                                          (second stack) (first stack))
                                 (drop 3 stack))))

              ;; Constants:
              ((T NIL -1 0 1 2)
               (push (opcode instr) stack))

              ;; Other:
              ((HALT) (setf halted? t))
              (otherwise (error "Unknown opcode: ~a" instr))))))))

(defparameter *default-bardvm* (make-instance 'vm))

(defun bard ()
  (init-bard-comp)
  (runvm *default-bardvm* (compiler bard-top-level)))

#+repl (bard)
