;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       the Bard compiler:
;;;;                ast -> Lisp code
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ------------------------------------------------------------
;;; Expression compilers
;;; ------------------------------------------------------------

(defmethod compile ((exp bint::expression)(env bint::environment))
  (error "Unrecognized expression: ~s" exp))

;;; Simple expressions
;;; ------------------------------------------------------------

;;; Void -> bard::void
(defmethod compile ((exp bint::void-expression)(env bint::environment))
  (void))

;;; Number -> CL:NUMBER
(defmethod compile ((exp bint::number-expression)(env bint::environment))
  (bint::value exp))

;;; Character -> CL::CHARACTER
(defmethod compile ((exp bint::character-expression)(env bint::environment))
  (bint::value exp))

;;; Keyword -> CL::Keyword
(defmethod compile ((exp bint::keyword-expression)(env bint::environment))
  (cl:intern (bint::value exp)
             (find-package :keyword)))

;;; True -> CL::T
(defmethod compile ((exp bint::true-expression)(env bint::environment))
  t)

;;; False -> CL::NIL
(defmethod compile ((exp bint::false-expression)(env bint::environment))
  nil)

;;; Text -> CL::STRING
(defmethod compile ((exp bint::text-expression)(env bint::environment))
  (bint::value exp))

;;; Symbols
;;; ------------------------------------------------------------
;;; Unquoted symbols are variable references

;;; Symbol -> CL::SYMBOL
(defmethod compile ((exp bint::symbol-expression)(env bint::environment))
  `(bint::lookup-variable-value (bint::intern ,exp ,env)
                                ,env))

;;; Sequences
;;; ------------------------------------------------------------
;;; Unquoted sequences are operations: quotations, special forms,
;;; macro calls and funcalls

;;; Empty Sequence
(defmethod compile ((exp bint::empty-sequence-expression)(env bint::environment))
  (empty-sequence))

;;; Sequence

;;; TODO: implement macros
(defun macro-symbol? (string)
  (declare (ignore string))
  nil)

(defmethod sequence-syntactic-type ((exp bint::sequence-expression))
  (let ((op (bint::first exp)))
    (cond
     ((and (bint::symbol? op)
           (equal "quote" (bint::value op))) :quotation)
     ((and (bint::symbol? op)
           (equal "define" (bint::value op))) :definition)
     ((and (bint::symbol? op)
           (equal "if" (bint::value op))) :if)
     ((and (bint::symbol? op)
           (equal "function" (bint::value op))) :function-literal)
     ((and (bint::symbol? op)
           (equal "fn" (bint::value op))) :method-literal)
     ((and (bint::symbol? op)
           (macro-symbol? (bint::value op))) :macro-call)
     (t :function-call))))

;;; BUG: this implementation quotes the abstract syntax expression, not the
;;; Bard expression it stands for.
(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :quotation))(env bint::environment))
  `(quote ,exp))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :definition))(env bint::environment))
  `(define-variable (bint::intern (bint::second ,exp))
                    (compile (bint::third ,exp)
                             ,env)
     ,env))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :if))(env bint::environment))
  `(if (true? (compile (bint::second ,exp) ,env))
     (compile (bint::third ,exp) ,env)
     (compile (bint::fourth ,exp) ,env)))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :function-literal))(env bint::environment))
  `(make-function ,exp ,env))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :method-literal))(env bint::environment))
  `(make-method ,exp ,env))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :macro-call))(env bint::environment))
  `(compile (expand-macro-call ,exp) ,env))

(defmethod compile-sequence ((exp bint::sequence-expression)(etype (eql :function-call))(env bint::environment))
  (let ((op (gensym "op-"))
        (arg (gensym "arg-"))
        (args (gensym "args-")))
    `(let ((,op (compile (bint::first ,exp) ,env))
           (,args (mapcar (lambda (,arg)
                            (compile ,arg ,env))
                          (bint::rest ,exp))))
       (apply-function ,op ,args ,env))))

(defmethod compile ((exp bint::sequence-expression)(env bint::environment))
  (compile-sequence exp (sequence-syntactic-type exp) env))

;;; Maps

;;; Empty Map
(defmethod compile ((exp bint::empty-map-expression)(env bint::environment))
  (empty-map))

;;; Map
(defmethod compile ((exp bint::map-expression)(env bint::environment))
  (let* ((exp-entries (bint::entries exp)))
    (if exp-entries
      (let* ((map-entries (mapcar (lambda (e)
                                   (let ((k (compile (car e) env))
                                         (v (compile (cdr e) env)))
                                     (cons k v)))
                                 exp-entries))
             (result (make-instance 'alist-map)))
        (setf (slot-value result 'entries) map-entries)
        result)
      (empty-map))))


;;; ----------------------------------------------------------------------
;;; testing
;;; ----------------------------------------------------------------------

(defun compile-test (s)
  (let ((x (read s))
        (env (bint::extend-environment nil)))
    (format t "~%~s" (compile x env))))

(defparameter $test-expressions
  '(
    ;; text
    "\"foo bar\""
    ;; numbers
    "123.45" "1" "#b101"
    ;; characters
    "\\c" "\\1" "\\space"
    ;; boolean and void
    "true" "false" "void"
    ;; symbols
    "foo" "bard.lang/foo"
    ;; keywords
    ":foo" "foo:" ":foo:"
    ;; sequences
    "()" "(foo)" "(+ (- 5 2)(- 4 3))"
    ;; maps
    "{}" "{name: foo}" "{first: {a b} second: {c d}}"
    ))

(defun run-compile-test ()
  (terpri)
  (dolist (s $test-expressions)
    (compile-test s))
  (terpri))