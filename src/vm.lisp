;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; vm.lisp
;;;; the bard vm
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


(defstruct ret-addr method pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op)
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))

(defun comp-go (exp)
  "Compile and execute the expression."
  (machine (compiler `(exit ,exp))))

(defun machine (f)
  "Run the abstract machine on the code for f."
  (let* ((code (method-code f))
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
         (FJUMP  (if (null (pop stack)) (setf pc (arg1 instr))))
         (TJUMP  (if (pop stack) (setf pc (arg1 instr))))

         ;; Function call/return instructions:
         (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                      :method f :env env)
                       stack))
         (RETURN ;; return value is top of stack; ret-addr is second
          (setf f (ret-addr-method (second stack))
                code (method-code f)
                env (ret-addr-env (second stack))
                pc (ret-addr-pc (second stack)))
          ;; Get rid of the ret-addr, but keep the value
          (setf stack (cons (first stack) (rest2 stack))))
         (CALLJ  (pop env)                 ; discard the top frame
                 (setf f  (pop stack)
                       code (method-code f)
                       env (method-env f)
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
         (METHOD     (push (make-method :code (method-code (arg1 instr))
                                :env env) stack))
         (PRIM   (push (apply (arg1 instr)
                              (loop with args = nil repeat n-args
                                    do (push (pop stack) args)
                                    finally (return args)))
                       stack))

         ;; Continuation instructions:
         (SET-CC (setf stack (top stack)))
         (CC     (push (make-method
                         :env (list (vector stack))
                         :code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                 (LVAR 0 0) (RETURN)))
                       stack))

         ;; Nullary operations:
         ((%%READ %%NEWLINE) ; *** fix, gat, 11/9/92
          (push (funcall (opcode instr)) stack))

         ;; Unary operations:
         ((%%WRITE CAR CDR CADR COMPILER %%DISPLAY %%FIRST %%LIST1 NOT RANDOM %%REST)
          (push (funcall (opcode instr) (pop stack)) stack))

         ;; Binary operations:
         ((+ - * / < > <= >= /= = CONS %%LIST2 NAME! EQ EQUAL EQL)
          (setf stack (cons (funcall (opcode instr) (second stack)
                                     (first stack))
                            (rest2 stack))))

         ;; Ternary operations:
         (%%LIST3
          (setf stack (cons (funcall (opcode instr) (third stack)
                                     (second stack) (first stack))
                            (rest3 stack))))

         ;; Constants:
         ((T NIL -1 0 1 2)
          (push (opcode instr) stack))

         ;; Other:
         ((HALT) (RETURN (top stack)))
         (otherwise (error "Unknown opcode: ~a" instr))))))

(defclass vm ()
  ((method :accessor vm-method :initform nil )
   (code :accessor vm-code :initform nil )
   (pc :accessor vm-pc :initform 0)
   (env :accessor vm-env :initform nil )
   (stack :accessor vm-stack :initform nil )
   (nargs :accessor vm-nargs :initform 0)
   (instr :accessor vm-instr :initform nil )))

(defmethod step-vm ((vm vm))
  (setf (vm-instr vm) (elt (vm-code vm) (vm-pc vm)))
  (incf (vm-pc vm))
  (case (opcode (vm-instr vm))

    ;; Variable/stack manipulation instructions:
    (LVAR   (push (elt (elt (vm-env vm) (arg1 (vm-instr vm))) (arg2 (vm-instr vm)))
                  (vm-stack vm)))
    (LSET   (setf (elt (elt (vm-env vm) (arg1 (vm-instr vm))) (arg2 (vm-instr vm)))
                  (top (vm-stack vm))))
    (GVAR   (push (get (arg1 (vm-instr vm)) 'global-val) (vm-stack vm)))
    (GSET   (setf (get (arg1 (vm-instr vm)) 'global-val) (top (vm-stack vm))))
    (POP    (pop (vm-stack vm)))
    (CONST  (push (arg1 (vm-instr vm)) (vm-stack vm)))

    ;; Branching instructions:
    (JUMP   (setf (vm-pc vm) (arg1 (vm-instr vm))))
    (FJUMP  (if (null (pop (vm-stack vm))) (setf (vm-pc vm) (arg1 (vm-instr vm)))))
    (TJUMP  (if (pop (vm-stack vm)) (setf (vm-pc vm) (arg1 (vm-instr vm)))))

    ;; Function call/return instructions:
    (SAVE   (push (make-ret-addr :pc (arg1 (vm-instr vm))
                                 :method (vm-method vm) :env (vm-env vm))
                  (vm-stack vm)))
    (RETURN ;; return value is top of stack; ret-addr is second
      (setf (vm-method vm) (ret-addr-method (second (vm-stack vm)))
            (vm-code vm) (method-code (vm-method vm))
            (vm-env vm) (ret-addr-env (second (vm-stack vm)))
            (vm-pc vm) (ret-addr-pc (second (vm-stack vm))))
      ;; Get rid of the ret-addr, but keep the value
      (setf (vm-stack vm) (cons (first (vm-stack vm)) (rest2 (vm-stack vm)))))
    (CALLJ  (pop (vm-env vm))                 ; discard the top frame
            (setf (vm-method vm)  (pop (vm-stack vm))
                  (vm-code vm) (method-code (vm-method vm))
                  (vm-env vm) (method-env (vm-method vm))
                  (vm-pc vm) 0
                  (vm-nargs vm) (arg1 (vm-instr vm))))
    (ARGS   (assert (= (vm-nargs vm) (arg1 (vm-instr vm))) ()
                    "Wrong number of arguments:~
                         ~d expected, ~d supplied"
                    (arg1 (vm-instr vm)) (vm-nargs vm))
            (push (make-array (arg1 (vm-instr vm))) (vm-env vm))
            (loop for i from (- (vm-nargs vm) 1) downto 0 do
                 (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))
    (ARGS.  (assert (>= (vm-nargs vm) (arg1 (vm-instr vm))) ()
                    "Wrong number of arguments:~
                         ~d or more expected, ~d supplied"
                    (arg1 (vm-instr vm)) (vm-nargs vm))
            (push (make-array (+ 1 (arg1 (vm-instr vm))) :initial-element nil) (vm-env vm))
            (loop repeat (- (vm-nargs vm) (arg1 (vm-instr vm))) do
                 (push (pop (vm-stack vm)) (elt (first (vm-env vm)) (arg1 (vm-instr vm)))))
            (loop for i from (- (arg1 (vm-instr vm)) 1) downto 0 do
                 (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))
    (METHOD     (push (make-method :code (method-code (arg1 (vm-instr vm)))
                                   :env (vm-env vm)) (vm-stack vm)))
    (PRIM   (push (apply (arg1 (vm-instr vm))
                         (loop with args = nil repeat (vm-nargs vm)
                            do (push (pop (vm-stack vm)) args)
                            finally (return args)))
                  (vm-stack vm)))

    ;; Continuation instructions:
    (SET-CC (setf (vm-stack vm) (top (vm-stack vm))))
    (CC     (push (make-method
                   :env (list (vector (vm-stack vm)))
                   :code '((ARGS 1) (LVAR 1 0 ";" (vm-stack vm)) (SET-CC)
                           (LVAR 0 0) (RETURN)))
                  (vm-stack vm)))

    ;; Nullary operations:
    ((%%READ %%NEWLINE) ; *** fix, gat, 11/9/92
     (push (funcall (opcode (vm-instr vm))) (vm-stack vm)))

    ;; Unary operations:
    ((%%WRITE CAR CDR CADR COMPILER %%DISPLAY %%FIRST %%LIST1 NOT RANDOM %%REST)
     (push (funcall (opcode (vm-instr vm)) (pop (vm-stack vm))) (vm-stack vm)))

    ;; Binary operations:
    ((+ - * / < > <= >= /= = CONS %%LIST2 NAME! EQ EQUAL EQL)
     (setf (vm-stack vm) (cons (funcall (opcode (vm-instr vm)) (second (vm-stack vm))
                                        (first (vm-stack vm)))
                               (rest2 (vm-stack vm)))))

    ;; Ternary operations:
    (%%LIST3
     (setf (vm-stack vm) (cons (funcall (opcode (vm-instr vm)) (third (vm-stack vm))
                                        (second (vm-stack vm)) (first (vm-stack vm)))
                               (rest3 (vm-stack vm)))))

    ;; Constants:
    ((T NIL -1 0 1 2)
     (push (opcode (vm-instr vm)) (vm-stack vm)))

    ;; Other:
    ((HALT) (throw :halt (top (vm-stack vm))))
    (otherwise (error "Unknown opcode: ~a" (vm-instr vm)))))

(defmethod run-vm ((vm vm))
  (if (typep (vm-method vm) 'method)
      (progn
        (catch :halt
          (loop (step-vm vm)))
        (top (vm-stack vm)))
      (progn (warn "No method loaded")
             vm)))

