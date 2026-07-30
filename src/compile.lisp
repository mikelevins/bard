;;;; ***********************************************************************
;;;;
;;;; Name:          compile.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the bootstrap compiler
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; A form is compiled in one of three contexts, and the context decides
;;; what happens to the values it produces:
;;;
;;;   :tail    the form's values are the enclosing method's values. A
;;;            call becomes op_TAILCALL and takes no receiver; anything
;;;            else ends in op_RETURN.
;;;   :value   the form's value is wanted. A call takes op_RECV 1.
;;;   :effect  the value is discarded. A call takes op_RECV 0; anything
;;;            else is followed by op_DROP.
;;;
;;; Those three cover the whole multiple-value surface with no special
;;; rule: values propagate in tail position, truncate to the first in
;;; value position, and are discarded in effect position. The top level
;;; of a repl interaction is tail position, which is why (values 1 2 3)
;;; typed at a prompt prints three values.
;;;
;;; Tail position is not an optimization. op_TAILCALL is semantics, so
;;; the analysis is required rather than deferred.
;;;
;;; This compiler emits assembly and hands it to ASSEMBLE. It is the
;;; bootstrap compiler; the real one is written in Bard.

;;; ---------------------------------------------------------------------
;;; emitting
;;; ---------------------------------------------------------------------
;;; Instructions accumulate in reverse. *DEPTH* tracks the operand area
;;; as they are emitted so that FRAME-SIZE can be computed rather than
;;; guessed.

(defvar *code*)
(defvar *depth*)
(defvar *max-depth*)

(defparameter +return-headroom+ 8
  "Slots reserved above the computed operand depth.

A call leaves its callee's values and their count in this frame, and how
many values that is depends on the callee, which the compiler does not
know. Until it can -- which needs a closed world -- this covers a callee
returning up to this many values. A callee returning more overruns the
frame.")

(defun stack-effect (instruction)
  "How INSTRUCTION changes the operand depth. A call is modelled as
returning one value, which the receiver that must follow it then
adjusts."
  (destructuring-bind (op &optional a b) instruction
    (declare (ignore b))
    (let ((name (string op)))
      (cond ((member name '("OP_CONST" "OP_LOCAL" "OP_GLOBAL" "OP_CLOSE")
                     :test #'string=)
             1)
            ((member name '("OP_SET-LOCAL" "OP_SET-GLOBAL" "OP_GOTO" "OP_YIELD")
                     :test #'string=)
             0)
            ((member name '("OP_DROP" "OP_BRANCH-FALSE" "OP_RECV-ALL")
                     :test #'string=)
             -1)
            ((string= name "OP_CALL") (- 1 a))
            ((string= name "OP_RECV") (- a 2))
            ;; op_TAILCALL and op_RETURN transfer control; what the depth
            ;; would have been afterwards does not arise.
            (t 0)))))

(defun emit (instruction)
  (push instruction *code*)
  (when (consp instruction)
    (incf *depth* (stack-effect instruction))
    (setf *max-depth* (max *max-depth* *depth*)))
  instruction)

(defun finish-value (context)
  "Dispose of the single value the form just left on the stack."
  (ecase context
    (:value)
    (:effect (emit '(op_DROP)))
    (:tail (emit '(op_RETURN 1)))))

;;; ---------------------------------------------------------------------
;;; the compile-time environment
;;; ---------------------------------------------------------------------
;;; A list of frames, innermost first, each a list of names. Names are
;;; compared as strings for the same reason global bindings are: a
;;; program should read the same whichever package it was typed in.

(defun lookup (name env)
  "UP and SLOT if NAME is lexical here, or NIL if it is global."
  (loop for frame in env
        for up from 0
        for slot = (position (string name) frame :key #'string :test #'string=)
        when slot
          do (return (values up slot))))

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(defparameter *special-forms* (make-hash-table :test #'equal)
  "Name string to compiler function.")

(defmacro define-special-form (name (form env context) &body body)
  `(setf (gethash ,(string name) *special-forms*)
         (lambda (,form ,env ,context)
           (declare (ignorable ,form ,env ,context))
           ,@body)))

(defun special-form-compiler (head)
  (and (symbolp head) (gethash (string head) *special-forms*)))

;;; ---------------------------------------------------------------------
;;; compiling
;;; ---------------------------------------------------------------------

(defun comp (form env context)
  (cond ((and (symbolp form) form (not (keywordp form)))
         (comp-variable form env context))
        ((atom form)
         (comp-constant form context))
        ((macro-function-for (first form))
         (comp (macroexpand-form form) env context))
        (t
         (let ((compiler (special-form-compiler (first form))))
           (if compiler
               (funcall compiler form env context)
               (comp-call form env context))))))

(defun comp-constant (value context)
  (emit `(op_CONST ,value))
  (finish-value context))

(defun comp-variable (name env context)
  (multiple-value-bind (up slot) (lookup name env)
    (if up
        (emit `(op_LOCAL ,up ,slot))
        (emit `(op_GLOBAL ,name))))
  (finish-value context))

(defun comp-call (form env context)
  (let ((n (length (rest form))))
    (dolist (argument (rest form))
      (comp argument env :value))
    (comp (first form) env :value)
    (cond ((eq context :tail)
           (emit `(op_TAILCALL ,n)))
          (t
           (emit `(op_CALL ,n))
           (emit (if (eq context :effect) '(op_RECV 0) '(op_RECV 1)))))))

(define-special-form quote (form env context)
  (comp-constant (second form) context))

(define-special-form begin (form env context)
  (let ((forms (rest form)))
    (if (null forms)
        (comp-constant *nothing* context)
        (loop for (this . more) on forms
              do (comp this env (if more :effect context))))))

(define-special-form set! (form env context)
  (destructuring-bind (place value) (rest form)
    (comp value env :value)
    (multiple-value-bind (up slot) (lookup place env)
      (if up
          (emit `(op_SET-LOCAL ,up ,slot))
          (emit `(op_SET-GLOBAL ,place))))
    (finish-value context)))

(define-special-form if (form env context)
  (destructuring-bind (test then &optional else) (rest form)
    (let ((else-label (gensym "ELSE"))
          (done-label (gensym "DONE"))
          (entry-depth 0))
      (comp test env :value)
      (emit `(op_BRANCH-FALSE ,else-label))
      (setf entry-depth *depth*)
      (comp then env context)
      ;; In tail context both arms return, so there is nothing to join.
      (let ((after-then *depth*))
        (unless (eq context :tail)
          (emit `(op_GOTO ,done-label)))
        (setf *depth* entry-depth)
        (emit else-label)
        (comp else env context)
        (unless (eq context :tail)
          (emit done-label))
        (setf *depth* (max after-then *depth*))))))

(define-special-form values (form env context)
  (let ((arguments (rest form)))
    (ecase context
      (:tail
       (dolist (argument arguments)
         (comp argument env :value))
       (emit `(op_RETURN ,(length arguments))))
      (:value
       ;; Truncate to the first, as Common Lisp does, while still
       ;; evaluating the rest in order.
       (cond ((null arguments) (comp-constant *nothing* :value))
             (t (comp (first arguments) env :value)
                (dolist (argument (rest arguments))
                  (comp argument env :effect)))))
      (:effect
       (dolist (argument arguments)
         (comp argument env :effect))))))

(define-special-form method (form env context)
  (destructuring-bind (parameters &rest body) (rest form)
    (emit `(op_CLOSE ,(compile-method parameters body env))))
  (finish-value context))

;;; ---------------------------------------------------------------------
;;; quasiquote
;;; ---------------------------------------------------------------------
;;; The reader turns ` , and ,@ into (quasiquote x), (unquote x), and
;;; (unquote-splicing x) -- see *BARD-READTABLE*. This expands those into
;;; ordinary list construction, after Norvig, so that a template is
;;; nothing but code the compiler already understands.

(defun quasi (form)
  "Expand a quasiquoted FORM into Bard code that builds it."
  (cond ((named-form-p form "UNQUOTE") (second form))
        ((named-form-p form "UNQUOTE-SPLICING")
         (error "~S is not inside a list." form))
        ((atom form) (list 'quote form))
        ((named-form-p (first form) "UNQUOTE-SPLICING")
         (if (null (rest form))
             (second (first form))
             (list '_append (second (first form)) (quasi (rest form)))))
        (t (list '_cons (quasi (first form)) (quasi (rest form))))))

(defun named-form-p (form name)
  (and (consp form)
       (symbolp (first form))
       (string= (string (first form)) name)))

(define-special-form quasiquote (form env context)
  (comp (quasi (second form)) env context))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------
;;; Unhygienic, defmacro-style: a macro is an ordinary method, compiled
;;; and run on the machine, whose result is compiled in its place. The
;;; expander runs during compilation, which is the first place the
;;; machine is used to build Bard rather than merely to run it.

(defparameter *macros* (make-hash-table :test #'equal)
  "Name string to the method that expands it.")

(defun macro-function-for (head)
  (and (symbolp head) (gethash (string head) *macros*)))

(defun macroexpand-form (form)
  "Apply FORM's macro to its unevaluated arguments and return the result."
  (let ((expander (macro-function-for (first form))))
    (first (run (apply-frame expander (rest form))))))

(defun apply-frame (fn arguments)
  "A frame that applies FN to ARGUMENTS, with no parent, ready to run."
  (let* ((frame (make-frame fn :parent nil))
         (slots (frame-slots frame)))
    (loop for argument in arguments
          for i from 0
          do (setf (svref slots i) argument))
    frame))

(define-special-form defmacro (form env context)
  (destructuring-bind (name parameters &rest body) (rest form)
    (setf (gethash (string name) *macros*)
          (make-fn (compile-method parameters body '() :name (string name))))
    (comp-constant name context)))

;;; ---------------------------------------------------------------------
;;; entry points
;;; ---------------------------------------------------------------------

(defun parse-parameters (parameters)
  "Values: the required names, and the rest name or NIL.

A symbol takes everything: (method args ...). A dotted list takes a
fixed prefix and the remainder: (method (a b . more) ...)."
  (cond ((symbolp parameters)
         (if (null parameters)
             (values '() nil)
             (values '() parameters)))
        (t (loop for tail = parameters then (cdr tail)
                 while (consp tail)
                 collect (car tail) into required
                 finally (return (values required tail))))))

(defun compile-method (parameters body env &key (name "method"))
  "Compile BODY as a method of PARAMETERS, closed over ENV. Returns a
code object."
  (multiple-value-bind (required rest) (parse-parameters parameters)
    (let ((*code* '())
          (*depth* 0)
          (*max-depth* 0)
          (slots (if rest (append required (list rest)) required)))
      (let ((inner (cons slots env)))
        (if (null body)
            (comp *nothing* inner :tail)
            (loop for (this . more) on body
                  do (comp this inner (if more :effect :tail)))))
      (assemble (nreverse *code*)
                :name name
                :arity (length required)
                :rest? (and rest t)
                :n-locals (length slots)
                :frame-size (+ (length slots) *max-depth* +return-headroom+)))))

(defun compile-form (form &key (name "toplevel"))
  "Compile FORM as a whole computation. Its context is tail, so all of
its values are delivered."
  (compile-method '() (list form) '() :name name))

(defun eval-form (form)
  "Compile and run FORM, returning its values as a list."
  (run-code (compile-form form)))

(defparameter *bard-readtable*
  (let ((table (copy-readtable nil)))
    (set-macro-character
     #\` (lambda (stream char)
            (declare (ignore char))
            (list 'quasiquote (read stream t nil t)))
     nil table)
    (set-macro-character
     #\, (lambda (stream char)
            (declare (ignore char))
            (if (char= (peek-char nil stream) #\@)
                (progn (read-char stream)
                       (list 'unquote-splicing (read stream t nil t)))
                (list 'unquote (read stream t nil t))))
     nil table)
    table)
  "The host's reader, with ` and , producing forms the compiler can see
rather than an implementation-specific structure. This is the whole of
the Bard reader for now, and where auto-gensym will go.")

(defun load-bard-file (pathname)
  "Read PATHNAME as Bard source and run each form in order. Returns the
values of the last one.

The bootstrap borrows the host's reader: Bard's surface is
s-expressions, so this works, at the cost of the host's case rules,
package semantics, and read syntax. The Bard reader is a later rung."
  (with-open-file (stream pathname)
    (let ((*package* (find-package :bard))
          (*readtable* *bard-readtable*)
          (result '()))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (setf result (eval-form form)))
      result)))

#+repl (bard:disassemble (compile-form '(if (< 1 2) "small" "big")))
#+repl (eval-form '(+ 2 3))                       ; => (5)
#+repl (eval-form '(values 1 2 3))                ; => (1 2 3)
#+repl (eval-form '((method (x) (* x x)) 7))      ; => (49)
