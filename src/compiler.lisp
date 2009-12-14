;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ----------------------------------------------------------------------
;;; NOTE
;;; ----------------------------------------------------------------------
;;; a cheap hack for in-ram compilation (unless I think of a better way 
;;; that is equally easy and quick to implement): the compiler can
;;; compile a Bard AST to LISP s-expressions, format the result to a 
;;; string, and then load the resulting string using:
;;; (with-input-from-string (in compiler-output)
;;;     (load in))
;;; cheap hack, but it works, and will be very quick and easy as
;;; a short-term solution

;;; ----------------------------------------------------------------------
;;; expression types
;;; ----------------------------------------------------------------------
;;; macro-call
;;; quotation
;;; definition
;;; conditional-expression
;;; apply-expression
;;; function-expression
;;; function-call
;;; call/cc

;;; ----------------------------------------------------------------------
;;; the compiler
;;; ----------------------------------------------------------------------

;;; true and false
(defmethod compile ((exp true-expression)(env environment)) (generate-true))
(defmethod compile ((exp false-expression)(env environment)) (generate-false))

;;; references to variables
(defmethod compile ((exp variable-reference)(env environment))
  (let ((binding (find-binding exp env)))
    (if binding
        (generate-lexical-reference binding)
        (generate-module-reference exp))))

;;; atoms other than references to variables
(defmethod compile ((exp atomic-expression)(env environment))
  (generate-constant exp))

;;; macro calls
(defmethod compile ((exp macro-call)(env environment))
  (compile (expand-macro exp) env))

;;; quotations
(defmethod compile ((exp quotation)(env environment))
  (generate-constant (value exp)))

;;; sequences of expressions ('begin' forms)
(defmethod compile ((exp expression-sequence)(env environment))
  (if (empty? exp)
      (generate-void)
      (apply (function code-sequence)
             (map-over (partial (flip (function compile)) env) exp))))

;;; definitions
(defmethod compile ((exp definition)(env environment))
  (let ((x (validate-definition exp)))
    (generate-definition (variable x)(value x))))

;;; conditionals
(defmethod compile ((exp conditional)(env environment))
  (generate-conditional (test-expression exp)(then-branch exp)(else-branch exp)))

;;; apply expressions
(defmethod compile ((exp apply-expression)(env environment))
  (generate-apply (compile (operator exp) env)
                  (map (partial (flip (function compile)) env)
                       (arguments exp))))

;;; function expressions
(defmethod compile ((exp function-expression)(env environment))
  (generate-function (arguments exp)
                     (compile (body exp) env)))

;;; function calls
(defmethod compile ((exp function-call)(env environment))
  (generate-function-call (compile (operator exp) env)
                          (map (partial (flip (function compile)) env)
                               (body exp))))
