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
  ((fn :accessor fn :initform nil :initarg :fn)
   (code :accessor code :initform nil :initarg :code)
   (pc :accessor pc :initform 0 :initarg :pc)
   (env :accessor env :initform nil :initarg :env)
   (stack :accessor stack :initform nil :initarg :stack)
   (n-args :accessor n-args :initform 0 :initarg :n-args)
   (instr :accessor instr :initform nil :initarg :instr)
   (halted? :accessor halted? :initform t :initarg :halted)))

(defmethod vm-load ((vm vm)(fn fn))
  (setf (fn vm) fn
        (code vm) (fn-code fn)
        (pc vm) 0
        (env vm) nil
        (stack vm) nil
        (n-args vm) 0
        (instr vm) nil
        (halted? vm) nil)
  vm)

(defmethod stepvm ((vm vm)(fn fn))
  (progn
    (setf (instr vm) (elt (code vm) (pc vm)))
    (incf (pc vm))
    (case (opcode (instr vm))

      ;; Variable/stack manipulation instructions:
      (LVAR   (push (elt (elt (env vm) (arg1 (instr vm))) (arg2 (instr vm)))
                    (stack vm)))
      (LSET   (setf (elt (elt (env vm) (arg1 (instr vm))) (arg2 (instr vm)))
                    (top (stack vm))))
      (GVAR   (push (get (arg1 (instr vm)) 'global-val) (stack vm)))
      (GSET   (setf (get (arg1 (instr vm)) 'global-val) (top (stack vm))))
      (POP    (pop (stack vm)))
      (CONST  (push (arg1 (instr vm)) (stack vm)))

      ;; Branching instructions:
      (JUMP   (setf (pc vm) (arg1 (instr vm))))
      (FJUMP  (if (null (pop (stack vm))) (setf (pc vm) (arg1 (instr vm)))))
      (TJUMP  (if (pop (stack vm)) (setf (pc vm) (arg1 (instr vm)))))

      ;; Function call/return instructions:
      (SAVE   (push (make-ret-addr :pc (arg1 (instr vm))
                                   :fn (fn vm) :env (env vm))
                    (stack vm)))
      (RETURN ;; return value is top of stack; ret-addr is second
        (setf (fn vm) (ret-addr-fn (second (stack vm)))
              (code vm) (fn-code (fn vm))
              (env vm) (ret-addr-env (second (stack vm)))
              (pc vm) (ret-addr-pc (second (stack vm))))
        ;; Get rid of the ret-addr, but keep the value
        (setf (stack vm) (cons (first (stack vm)) (drop 2 (stack vm)))))
      (CALLJ  (pop (env vm))                 ; discard the top frame
       (setf (fn vm)  (pop (stack vm))
             (code vm) (fn-code (fn vm))
             (env vm) (fn-env (fn vm))
             (pc vm) 0
             (n-args vm) (arg1 (instr vm))))
      (ARGS   (assert (= (n-args vm) (arg1 (instr vm))) ()
                      "Wrong number of arguments:~
                         ~d expected, ~d supplied"
                      (arg1 (instr vm)) (n-args vm))
       (push (make-array (arg1 (instr vm))) (env vm))
       (loop for i from (- (n-args vm) 1) downto 0 do
         (setf (elt (first (env vm)) i) (pop (stack vm)))))
      (ARGS.  (assert (>= (n-args vm) (arg1 (instr vm))) ()
                      "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
                      (arg1 (instr vm)) (n-args vm))
       (push (make-array (+ 1 (arg1 (instr vm)))) (env vm))
       (loop repeat (- (n-args vm) (arg1 (instr vm))) do
         (push (pop (stack vm)) (elt (first (env vm)) (arg1 (instr vm)))))
       (loop for i from (- (arg1 (instr vm)) 1) downto 0 do
         (setf (elt (first (env vm)) i) (pop (stack vm)))))
      (FN     (push (make-fn :code (fn-code (arg1 (instr vm)))
                             :env (env vm))
                    (stack vm)))
      (PRIM   (push (apply (arg1 (instr vm))
                           (loop with args = nil repeat (n-args vm)
                                 do (push (pop (stack vm)) args)
                                 finally (return args)))
                    (stack vm)))

      ;; Continuation instructions:
      (SET-CC (setf (stack vm) (top (stack vm))))
      (CC     (push (make-fn
                     :env (list (vector (stack vm)))
                     :code '((ARGS 1) (LVAR 1 0 ";" (stack vm)) (SET-CC)
                             (LVAR 0 0) (RETURN)))
                    (stack vm)))

      ;; Nullary operations:
      ((BARD-READ NEWLINE) ; *** fix, gat, 11/9/92
       (push (funcall (opcode (instr vm))) (stack vm)))

      ;; Unary operations:
      ((CAR CDR CADR EOF-OBJECT? NOT LIST1 COMPILER DISPLAY WRITE RANDOM)
       (push (funcall (opcode (instr vm)) (pop (stack vm))) (stack vm)))

      ;; Binary operations:
      ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
       (setf (stack vm) (cons (funcall (opcode (instr vm)) (second (stack vm))
                                       (first (stack vm)))
                              (drop 2 (stack vm)))))

      ;; Ternary operations:
      (LIST3
       (setf (stack vm) (cons (funcall (opcode (instr vm)) (third (stack vm))
                                       (second (stack vm)) (first (stack vm)))
                              (drop 3 (stack vm)))))

      ;; Constants:
      ((T NIL -1 0 1 2)
       (push (opcode (instr vm)) (stack vm)))

      ;; Other:
      ((HALT) (setf (halted? vm) t))
      (otherwise (error "Unknown opcode: ~a" (instr vm))))))

(defmethod runvm ((vm vm)(fn fn))
  (vm-load vm fn)
  (loop
    (if (halted? vm)
        ;; stop the vm
        (return (top (stack vm)))
        ;; step the vm
        (stepvm vm fn))))

(defparameter *default-bardvm* (make-instance 'vm))

(defun bard ()
  (init-bard-comp)
  (runvm *default-bardvm* (compiler bard-top-level)))

#+repl (bard)

