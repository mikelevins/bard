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

(defclass <vm> ()
  ((mfn :accessor vm-mfn :initform nil :initarg :mfn)
   (code :accessor vm-code :initform nil :initarg :code)
   (pc :accessor vm-pc :initform nil :initarg :pc)
   (env :accessor vm-env :initform nil :initarg :env)
   (stack :accessor vm-stack :initform nil :initarg :stack)
   (n-args :accessor vm-n-args :initform nil :initarg :n-args)
   (instr :accessor vm-instr :initform nil :initarg :instr)))

(defmethod initialize-instance :after ((vm <vm>) 
                                       &rest initargs
                                       &key (mfn nil)
                                         &allow-other-keys)
  (setf (vm-mfn vm) mfn)
  (setf (vm-code vm) (if mfn
                         (mfn-code mfn)
                         nil))
  (setf (vm-pc vm) 0)
  (setf (vm-env vm) (null-environment))
  (setf (vm-stack vm) nil)
  (setf (vm-n-args vm) 0)
  (setf (vm-instr vm) nil))

(defclass ret-addr () 
  ((fn :accessor ret-addr-fn :initform nil :initarg :fn)
   (pc :accessor ret-addr-pc :initform nil :initarg :pc)
   (env :accessor ret-addr-env :initform nil :initarg :env)))

(defmethod vm-stack-top ((vm <vm>))
  (first (vm-stack vm)))

(defmethod vmstep ((vm <vm>))
  
  (setf (vm-instr vm) (elt (vm-code vm) (vm-pc vm)))
  (incf (vm-pc vm))

  (let ((instr (vm-instr vm)))
    (case (opcode instr)
      
      ;; Variable/stack manipulation instructions:
      (LREF   (push (elt (elt env (arg1 instr)) (arg2 instr))
                    (vm-stack vm)))
      (LSET   (setf (elt (elt env (arg1 instr)) (arg2 instr))
                    (vm-stack-top vm)))
      (GREF   (let* ((var (arg1 instr))
                     (mname (module-name var))
                     (vname (name var)))
                (push (get-module-variable mname vname)
                      (vm-stack vm))))
      (GSET   (let* ((var (arg1 instr))
                     (mname (module-name var))
                     (vname (name var)))
                (set-module-variable! mname vname (vm-stack-top vm))))
      (POP    (pop (vm-stack vm)))
      (CONST  (push (arg1 instr) (vm-stack vm)))
      
      ;; Branching instructions:
      (GO   (setf (vm-pc vm) (arg1 instr)))
      (FGO  (if (false? (pop (vm-stack vm))) (setf (vm-pc vm) (arg1 instr))))
      (TGO  (if (true? (pop (vm-stack vm))) (setf (vm-pc vm) (arg1 instr))))
      
      ;; Function call/return instructions:
      (SAVE   (push (make-ret-addr :pc (arg1 instr)
                                   :fn (vm-mfn vm) :env (vm-env vm))
                    (vm-stack vm)))
      (RETURN ;; return value is top of stack; ret-addr is second
        (setf (vm-mfn vm) (ret-addr-fn (second (vm-stack vm)))
              (vm-code vm) (mfn-code (vm-mfn vm))
              (vm-env vm) (ret-addr-env (second (vm-stack vm)))
              (vm-pc vm) (ret-addr-pc (second (vm-stack vm))))
        ;; Get rid of the ret-addr, but keep the value
        (setf (vm-stack vm)
              (cons (first (vm-stack vm))
                    (rest2 (vm-stack vm)))))
      (CALLJ  (pop (vm-env vm))                 ; discard the top frame
              (setf (vm-mfn vm) (pop (vm-stack vm))
                    (vm-code vm) (mfn-code (vm-mfn vm))
                    (vm-env vm) (mfn-env (vm-mfn vm))
                    (vm-pc vm) 0
                    (vm-n-args vm) (arg1 instr)))
      (ARGS   (assert (= (vm-n-args vm) (arg1 instr)) ()
                      "Wrong number of arguments: ~d expected, ~d supplied"
                      (arg1 instr) (vm-n-args vm))
              (push (make-array (arg1 instr)) (vm-env vm))
              (loop for i from (- (vm-n-args vm) 1) downto 0 do
                   (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))
      (ARGS.  (assert (>= (vm-n-args vm) (arg1 instr)) ()
                      "Wrong number of arguments: ~d or more expected, ~d supplied"
                      (arg1 instr) (vm-n-args vm))
              (push (make-array (+ 1 (arg1 instr))) (vm-env vm))
              (loop repeat (- (vm-n-args vm) (arg1 instr)) do
                   (push (pop (vm-stack vm)) (elt (first (vm-env vm)) (arg1 instr))))
              (loop for i from (- (arg1 instr) 1) downto 0 do
                   (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))
      (MFN     (push (make-mfn :code (mfn-code (arg1 instr))
                               :env (vm-env vm)) (vm-stack vm)))
      (PRIM   (push (apply (arg1 instr)
                           (loop with args = nil repeat (vm-n-args vm)
                              do (push (pop (vm-stack vm)) args)
                              finally (return args)))
                    (vm-stack vm)))
      
      ;; Continuation instructions:
      (SET-CC (setf (vm-stack vm) (vm-stack-top vm)))
      (CC     (push (make-mfn
                     :env (list (vector (vm-stack vm)))
                     :code '((ARGS 1) (LREF 1 0 ";" stack) (SET-CC)
                             (LREF 0 0) (RETURN)))
                    (vm-stack vm)))
      
      ;; Nullary operations:
      ((BARD-READ NEWLINE)
       (push (funcall (opcode instr)) (vm-stack vm)))
      
      ;; Unary operations:
      ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM PRIMFIB) 
       (push (funcall (opcode instr) (pop (vm-stack vm))) (vm-stack vm)))
      
      ;; Binary operations:
      ((+ - * / < > <= >= /= = CONS LIST2 NAME! EQ EQUAL EQL)
       (setf (vm-stack vm)
             (cons (funcall (opcode instr) (second (vm-stack vm))
                            (first (vm-stack vm)))
                   (rest2 (vm-stack vm)))))
      
      ;; Ternary operations:
      (LIST3
       (setf (vm-stack vm)
             (cons (funcall (opcode instr) (third (vm-stack vm))
                            (second (vm-stack vm)) (first (vm-stack vm)))
                   (rest3 (vm-stack vm)))))
      
      ;; Constants:
      (TRUE (push (true) (vm-stack vm)))
      (FALSE (push (false) (vm-stack vm)))
      (UNDEFINED (push (undefined) (vm-stack vm)))
      (NOTHING (push (nothing) (vm-stack vm)))
      
      ;; Other:
      ((HALT) (vm-stack-top vm))
      (otherwise (error "Unknown opcode: ~a" instr)))))
