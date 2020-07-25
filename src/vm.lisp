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
          (loop (step-vm vm))))
      (progn (warn "No method loaded")
             vm)))


(defmethod machine ((m method))
  (let ((vm (make-instance 'vm)))
    (setf (vm-method vm) m)
    (setf (vm-code vm) (method-code m))
    (setf (vm-pc vm) 0)
    (run-vm vm)))
