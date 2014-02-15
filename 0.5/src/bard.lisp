;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       bard 0.5 implementation
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmacro read-time-case (first-case &rest other-cases)
  "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
  (declare (ignore other-cases))
  first-case)

(defun rest2 (x)
  "The rest of a list after the first TWO elements."
  (rest (rest x)))

(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(defun symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

;;; ==============================

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

;;; ==============================

(defun set-global-var! (var val)
  (setf (get var 'global-val) val))

(defun extend-env (vars vals env)
  "Add some variables and values to an environment."
  (nconc (mapcar #'list vars vals) env))

(defparameter *bard-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-bard-proc (f)
  "Define a Bard procedure as a corresponding CL function."
  (if (listp f)
      (set-global-var! (first f) (symbol-function (second f)))
      (set-global-var! f (symbol-function f))))

;;; ==============================

(defun bard-macro (symbol)
  (and (symbolp symbol) (get symbol 'bard-macro)))

(defmacro def-bard-macro (name parmlist &body body)
  "Define a Bard macro."
  `(setf (get ',name 'bard-macro)
         #'(lambda ,parmlist .,body)))

(defun bard-macro-expand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro (first x)))
      (bard-macro-expand
       (apply (bard-macro (first x)) (rest x)))
      x))

;;; ==============================

(def-bard-macro let (bindings &rest body)
  `((lambda ,(mapcar #'first bindings) . ,body)
    .,(mapcar #'second bindings)))

(def-bard-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin .,body)
      `(let (,(first bindings))
         (let* ,(rest bindings) . ,body))))

(def-bard-macro and (&rest args)
  (cond ((null args) 'T)
        ((length=1 args) (first args))
        (t `(if ,(first args)
                (and . ,(rest args))))))

(def-bard-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (first args))
        (t (let ((var (gensym)))
             `(let ((,var ,(first args)))
                (if ,var ,var (or . ,(rest args))))))))

(def-bard-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (first clauses))
         `(or ,(first clauses) (cond .,(rest clauses))))
        ((starts-with (first clauses) 'else)
         `(begin .,(rest (first clauses))))
        (t `(if ,(first (first clauses))
                (begin .,(rest (first clauses)))
                (cond .,(rest clauses))))))

(def-bard-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
       (cond ,@(mapcar
                #'(lambda (clause)
                    (if (starts-with clause 'else)
                        clause
                        `((member ,key-val ',(first clause))
                          .,(rest clause))))
                clauses)))))

(def-bard-macro define (name &rest body)
  (if (atom name)
      `(begin (set! ,name . ,body) ',name)
      `(define ,(first name) 
           (lambda ,(rest name) . ,body))))

(def-bard-macro delay (computation)
  `(lambda () ,computation))

(def-bard-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (first v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! .,v)) bindings)
     .,body))

;;; ==============================

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

;;; ==============================

(defvar *label-num* 0)

(defun compiler (x)
  "Compile an expression as if it were in a parameterless lambda."
  (setf *label-num* 0)
  (comp-lambda '() (list x) nil))

(defun comp-show (x)
  "Compile an expression and show the resulting code"
  (show-fn (compiler x))
  (values))

;;; ==============================

(defun gen (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun gen-label (&optional (label 'L))
  "Generate a label (a symbol of the form Lnnn)"
  (intern (format nil "~a~d" label (incf *label-num*))))

;;; ==============================

(defun gen-var (var env)
  "Generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LVAR (first p) (second p) ";" var)
        (gen 'GVAR var))))

;;; ==============================

(def-bard-macro define (name &rest body)
  (if (atom name)
      `(name! (set! ,name . ,body) ',name)
      (bard-macro-expand
       `(define ,(first name) 
            (lambda ,(rest name) . ,body)))))

(defun name! (fn name)
  "Set the name field of fn, if it is an un-named fn."
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)

(set-global-var! 'name! #'name!)

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "{~a}" (or (fn-name fn) '??)))

(defun label-p (x) "Is x a label?" (atom x))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (let ((frame (find symbol env :test #'find)))
    (if frame (list (position frame env) (position symbol frame)))))


(defun comp (x env val? more?)
  "Compile the expression x into a list of instructions"
  (cond
    ((member x '(t nil)) (comp-const x val? more?))
    ((symbolp x) (comp-var x env val? more?))
    ((atom x) (comp-const x val? more?))
    ((bard-macro (first x)) (comp (bard-macro-expand x) env val? more?))
    ((case (first x)
       (QUOTE  (arg-count x 1)
               (comp-const (second x) val? more?))
       (BEGIN  (comp-begin (rest x) env val? more?))
       (SET!   (arg-count x 2)
               (assert (symbolp (second x)) (x)
                       "Only symbols can be set!, not ~a in ~a"
                       (second x) x)
               (seq (comp (third x) env t t)
                    (gen-set (second x) env)
                    (if (not val?) (gen 'POP))
                    (unless more? (gen 'RETURN))))
       (IF     (arg-count x 2 3)
               (comp-if (second x) (third x) (fourth x)
                        env val? more?))
       (LAMBDA (when val?
                 (let ((f (comp-lambda (second x) (rest2 x) env)))
                   (seq (gen 'FN f) (unless more? (gen 'RETURN))))))
       (t      (comp-funcall (first x) (rest x) env val? more?))))))

;;; ==============================

(defun arg-count (form min &optional (max min))
  "Report an error if form has wrong number of args."
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
            "Wrong number of arguments for ~a in ~a: 
       ~d supplied, ~d~@[ to ~d~] expected"
            (first form) form n-args min (if (/= min max) max))))

;;; ==============================

(defun comp-begin (exps env val? more?)
  "Compile a sequence of expressions,
  returning the last one as the value."
  (cond ((null exps) (comp-const nil val? more?))
        ((length=1 exps) (comp (first exps) env val? more?))
        (t (seq (comp (first exps) env nil t)
                (comp-begin (rest exps) env val? more?)))))

(defun comp-list (exps env)
  "Compile a list, leaving them all on the stack."
  (if (null exps) nil
      (seq (comp (first exps) env t t)
           (comp-list (rest exps) env))))

;;; ==============================

(defun comp-const (x val? more?)
  "Compile a constant expression."
  (if val? (seq (if (member x '(t nil -1 0 1 2))
                    (gen x)
                    (gen 'CONST x))
                (unless more? (gen 'RETURN)))))

(defun comp-var (x env val? more?)
  "Compile a variable reference."
  (if val? (seq (gen-var x env) (unless more? (gen 'RETURN)))))

;;; ==============================

(defun comp-if (pred then else env val? more?)
  "Compile a conditional (IF) expression."
  (cond
    ((null pred)          ; (if nil x y) ==> y
     (comp else env val? more?))
    ((constantp pred)     ; (if t x y) ==> x
     (comp then env val? more?))
    ((and (listp pred)    ; (if (not p) x y) ==> (if p y x)
          (length=1 (rest pred))
          (primitive-p (first pred) env 1)
          (eq (prim-opcode (primitive-p (first pred) env 1)) 'not))
     (comp-if (second pred) else then env val? more?))
    (t (let ((pcode (comp pred env t t))
             (tcode (comp then env val? more?))
             (ecode (comp else env val? more?)))
         (cond
           ((equal tcode ecode) ; (if p x x) ==> (begin p x)
            (seq (comp pred env nil t) ecode))
           ((null tcode)  ; (if p nil y) ==> p (TJUMP L2) y L2:
            (let ((L2 (gen-label)))
              (seq pcode (gen 'TJUMP L2) ecode (list L2)
                   (unless more? (gen 'RETURN)))))
           ((null ecode)  ; (if p x) ==> p (FJUMP L1) x L1: 
            (let ((L1 (gen-label)))
              (seq pcode (gen 'FJUMP L1) tcode (list L1)
                   (unless more? (gen 'RETURN)))))
           (t             ; (if p x y) ==> p (FJUMP L1) x L1: y
                                        ; or p (FJUMP L1) x (JUMP L2) L1: y L2:
            (let ((L1 (gen-label))
                  (L2 (if more? (gen-label))))
              (seq pcode (gen 'FJUMP L1) tcode
                   (if more? (gen 'JUMP L2))
                   (list L1) ecode (if more? (list L2))))))))))

;;; ==============================

(defun comp-funcall (f args env val? more?)
  "Compile an application of a function to arguments."
  (let ((prim (primitive-p f env (length args))))
    (cond
      (prim  ; function compilable to a primitive instruction
       (if (and (not val?) (not (prim-side-effects prim)))
           ;; Side-effect free primitive when value unused
           (comp-begin args env nil more?)
           ;; Primitive with value or call needed
           (seq (comp-list args env)
                (gen (prim-opcode prim))
                (unless val? (gen 'POP))
                (unless more? (gen 'RETURN)))))
      ((and (starts-with f 'lambda) (null (second f)))
       ;; ((lambda () body)) => (begin body)
       (assert (null args) () "Too many arguments supplied")
       (comp-begin (rest2 f) env val? more?))
      (more? ; Need to save the continuation point
       (let ((k (gen-label 'k)))
         (seq (gen 'SAVE k)
              (comp-list args env)
              (comp f env t t)
              (gen 'CALLJ (length args))
              (list k)
              (if (not val?) (gen 'POP)))))
      (t     ; function call as rename plus goto
       (seq (comp-list args env)
            (comp f env t t)
            (gen 'CALLJ (length args)))))))

;;; ==============================

(defparameter *primitive-fns*
  '((+ 2 + true) (- 2 - true) (* 2 * true) (/ 2 / true)
    (< 2 <) (> 2 >) (<= 2 <=) (>= 2 >=) (/= 2 /=) (= 2 =)
    (eq? 2 eq) (equal? 2 equal) (eqv? 2 eql)
    (not 1 not) (null? 1 not)
    (car 1 car) (cdr 1 cdr)  (cadr 1 cadr) (cons 2 cons true)
    (list 1 list1 true) (list 2 list2 true) (list 3 list3 true)
    (read 0 bard-read nil t) (eof-object? 1 eof-object?) ;***
    (write 1 write nil t) (display 1 display nil t)
    (newline 0 newline nil t) (compiler 1 compiler t) 
    (name! 2 name! true t) (random 1 random true nil)))

;;; ==============================

(defstruct (prim (:type list)) 
  symbol n-args opcode always side-effects)

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-fns*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

(defun list1 (x) (list x))
(defun list2 (x y) (list x y))
(defun list3 (x y z) (list x y z))
(defun display (x) (princ x))
(defun newline () (terpri))

;;; ==============================

(defun gen-set (var env)
  "Generate an instruction to set a variable to top-of-stack."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LSET (first p) (second p) ";" var)
        (if (assoc var *primitive-fns*)
            (error "Can't alter the constant ~a" var)
            (gen 'GSET var)))))

;;; ==============================

(defun comp-lambda (args body env)
  "Compile a lambda form into a closure with compiled code."
  (new-fn :env env :args args
          :code (seq (gen-args args 0)
                     (comp-begin body
                                 (cons (make-true-list args) env)
                                 t nil))))

(defun gen-args (args n-so-far)
  "Generate an instruction to load the arguments."
  (cond ((null args) (gen 'ARGS n-so-far))
        ((symbolp args) (gen 'ARGS. n-so-far))
        ((and (consp args) (symbolp (first args)))
         (gen-args (rest args) (+ n-so-far 1)))
        (t (error "Illegal argument list"))))

(defun make-true-list (dotted-list)
  "Convert a possibly dotted list into a true, non-dotted list."
  (cond ((null dotted-list) nil)
        ((atom dotted-list) (list dotted-list))
        (t (cons (first dotted-list)
                 (make-true-list (rest dotted-list))))))

(defun new-fn (&key code env name args)
  "Build a new function."
  (assemble (make-fn :env env :name name :args args
                     :code (optimize code))))

;;; ==============================

(defun opcode (instr) (if (label-p instr) :label (first instr)))
(defun args (instr) (if (listp instr) (rest instr)))
(defun arg1 (instr) (if (listp instr) (second instr)))
(defun arg2 (instr) (if (listp instr) (third instr)))
(defun arg3 (instr) (if (listp instr) (fourth instr)))

(defsetf arg1 (instr) (val) `(setf (second ,instr) ,val))

;;; ==============================

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (fn-code fn))
    (setf (fn-code fn)
          (asm-second-pass (fn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (if (is instr '(JUMP TJUMP FJUMP SAVE))
            (setf (arg1 instr)
                  (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ==============================

(defun show-fn (fn &optional (stream *standard-output*) (indent 2))
  "Print all the instructions in a function.
  If the argument is not a function, just princ it, 
  but in a column at least 8 spaces wide."
  ;; This version handles code that has been assembled into a vector
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (dotimes (i (length (fn-code fn)))
          (let ((instr (elt (fn-code fn) i)))
            (if (label-p instr)
                (format stream "~a:" instr)
                (progn
                  (format stream "~VT~2d: " indent i)
                  (dolist (arg instr)
                    (show-fn arg stream (+ indent 8)))
                  (fresh-line))))))))

;;; ==============================

(defstruct ret-addr fn pc env)

(defun is (instr op)
  "True if instr's opcode is OP, or one of OP when OP is a list."
  (if (listp op) 
      (member (opcode instr) op)
      (eq (opcode instr) op)))

(defun top (stack) (first stack))

(defun machine (f)
  "Run the abstract machine on the code for f."
  (let* ((code (fn-code f))
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
                                      :fn f :env env)
                       stack))
         (RETURN ;; return value is top of stack; ret-addr is second
           (setf f (ret-addr-fn (second stack))
                 code (fn-code f)
                 env (ret-addr-env (second stack))
                 pc (ret-addr-pc (second stack)))
           ;; Get rid of the ret-addr, but keep the value
           (setf stack (cons (first stack) (rest2 stack))))
         (CALLJ  (pop env)                 ; discard the top frame
                 (setf f  (pop stack)
                       code (fn-code f)
                       env (fn-env f)
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
         ((CAR CDR CADR NOT LIST1 COMPILER DISPLAY WRITE RANDOM) 
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
         ((T NIL -1 0 1 2)
          (push (opcode instr) stack))
         
         ;; Other:
         ((HALT) (RETURN (top stack)))
         (otherwise (error "Unknown opcode: ~a" instr))))))


;;; ==============================

(defun init-bard-comp ()
  "Initialize values (including call/cc) for the Bard compiler."
  (set-global-var! 'exit 
                   (new-fn :name 'exit :args '(val) :code '((HALT))))
  (set-global-var! 'call/cc
                   (new-fn :name 'call/cc :args '(f)
                           :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
                                   (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
    (setf (get (prim-symbol prim) 'global-val)
          (new-fn :env nil :name (prim-symbol prim)
                  :code (seq (gen 'PRIM (prim-symbol prim))
                             (gen 'RETURN))))))

;;; ==============================

(defconstant bard-top-level
  '(begin (define (bard)
           (newline)
           (display "bard> ")
           (write ((compiler (read))))
           (bard))
    (bard)))

(defun bard ()
  "A compiled Bard read-eval-print loop"
  (init-bard-comp)
  (machine (compiler bard-top-level)))

(defun comp-go (exp)
  "Compile and execute the expression."
  (machine (compiler `(exit ,exp))))

;;;; Peephole Optimizer


;;; ==============================

(defun optimize (code)
  "Perform peephole optimization on assembly code."
  (let ((any-change nil))
    ;; Optimize each tail  
    (loop for code-tail on code do
         (setf any-change (or (optimize-1 code-tail code)
                              any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize code)
        code)))

;;; ==============================

(defun optimize-1 (code all-code)
  "Perform peephole optimization on a tail of the assembly code.
  If a change is made, return true."
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizer (get-optimizer (opcode instr))))
    (when optimizer
      (funcall optimizer instr code all-code))))

;;; ==============================

(let ((optimizers (make-hash-table :test #'eql)))

  (defun get-optimizer (opcode)
    "Get the assembly language optimizer for this opcode."
    (gethash opcode optimizers))

  (defun put-optimizer (opcode fn)
    "Store an assembly language optimizer for this opcode."
    (setf (gethash opcode optimizers) fn)))

;;; ==============================

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

;;; ==============================

(defmacro def-optimizer (opcodes args &body body)
  "Define assembly language optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))

;;;; Now for some additions and answers to exercises:

;;; ==============================

(defconstant eof "EoF")
(defun eof-object? (x) (eq x eof))
(defvar *bard-readtable* (copy-readtable))

;;; ==============================

(set-dispatch-macro-character #\# #\t 
                              #'(lambda (&rest ignore)(declare (ignore ignore)) t)
                              *bard-readtable*)

(set-dispatch-macro-character #\# #\f 
                              #'(lambda (&rest ignore)(declare (ignore ignore)) nil)
                              *bard-readtable*)

(set-dispatch-macro-character #\# #\d
                              ;; In both Common Lisp and Bard,
                              ;; #x, #o and #b are hexidecimal, octal, and binary,
                              ;; e.g. #xff = #o377 = #b11111111 = 255
                              ;; In Bard only, #d255 is decimal 255.
                              #'(lambda (stream &rest ignore)
                                  (declare (ignore ignore))
                                  (let ((*read-base* 10)) (bard-read stream)))
                              *bard-readtable*)

(set-macro-character #\` 
                     #'(lambda (s ignore)(declare (ignore ignore)) (list 'quasiquote (bard-read s))) 
                     nil *bard-readtable*)

(set-macro-character #\, 
                     #'(lambda (stream ignore)
                         (declare (ignore ignore))
                         (let ((ch (read-char stream)))
                           (if (char= ch #\@)
                               (list 'unquote-splicing (read stream))
                               (progn (unread-char ch stream)
                                      (list 'unquote (read stream))))))
                     nil *bard-readtable*)



;;; ==============================

                                        ;(setf (bard-macro 'quasiquote) 'quasi-q)

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)      
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

;;; ==============================

(defun bard-read (&optional (stream *standard-input*))
  (let ((*readtable* *bard-readtable*))
    (convert-numbers (read stream nil eof))))

(defun convert-numbers (x)
  "Replace symbols that look like Bard numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (cl:symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))

(def-optimizer (:LABEL) (instr code all-code)
  ;; ... L ... => ... ... ;if no reference to L
  (when (not (find instr all-code :key #'arg1))
    (setf (first code) (second code)
          (rest code) (rest2 code))
    t))

(def-optimizer (GSET LSET) (instr code all-code)
  (declare (ignore all-code))
  ;; ex: (begin (set! x y) (if x z))
  ;; (SET X) (POP) (VAR X) ==> (SET X)
  (when (and (is (second code) 'POP)
             (is (third code) '(GVAR LVAR))
             (eq (arg1 instr) (arg1 (third code))))
    (setf (rest code) (nthcdr 3 code))
    t))

(def-optimizer (JUMP CALL CALLJ RETURN) (instr code all-code)
  (declare (ignore all-code))
  ;; (JUMP L1) ...dead code... L2 ==> (JUMP L1) L2
  (setf (rest code) (member-if #'label-p (rest code)))
  ;; (JUMP L1) ... L1 (JUMP L2) ==> (JUMP L2)  ... L1 (JUMP L2)
  (when (and (is instr 'JUMP)
             (is (target instr code) '(JUMP RETURN))
             (setf (first code) (copy-list (target instr code)))
             t)))

(def-optimizer (TJUMP FJUMP) (instr code all-code)
    (declare (ignore all-code))
  ;; (FJUMP L1) ... L1 (JUMP L2) ==> (FJUMP L2) ... L1 (JUMP L2)
  (when (is (target instr code) 'JUMP)
    (setf (second instr) (arg1 (target instr code)))
    t))

(def-optimizer (T -1 0 1 2) (instr code all-code)
  (declare (ignore instr all-code))
  (case (opcode (second code))
    (NOT ;; (T) (NOT) ==> NIL
     (setf (first code) (gen1 'NIL)
           (rest code) (rest2 code))
     t)
    (FJUMP ;; (T) (FJUMP L) ... => ...
     (setf (first code) (third code)
           (rest code) (rest3 code))
     t)
    (TJUMP ;; (T) (TJUMP L) ... => (JUMP L) ...
     (setf (first code) (gen1 'JUMP (arg1 (next-instr code))))
     t)))

(def-optimizer (NIL) (instr code all-code)
  (declare (ignore instr all-code))
  (case (opcode (second code))
    (NOT ;; (NIL) (NOT) ==> T
     (setf (first code) (gen1 'T)
           (rest code) (rest2 code))
     t)
    (TJUMP ;; (NIL) (TJUMP L) ... => ...
     (setf (first code) (third code)
           (rest code) (rest3 code))
     t)
    (FJUMP ;; (NIL) (FJUMP L) ==> (JUMP L)
     (setf (first code) (gen1 'JUMP (arg1 (next-instr code))))
     t)))
