;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; code sequencer
;;; ---------------------------------------------------------------------

(defun seq (&rest code)
  (apply #'append code))

;;; ---------------------------------------------------------------------
;;; code generators
;;; ---------------------------------------------------------------------

(defvar *label-num* 0)

(defun gen (opcode &rest args)
  (list (cons opcode args)))

(defun gen-args (args n-so-far)
  (cond ((null args) (gen 'ARGS n-so-far))
        ((symbolp args) (gen 'ARGS. n-so-far))
        ((and (consp args) (symbolp (first args)))
         (gen-args (rest args) (+ n-so-far 1)))
        (t (error "Illegal argument list"))))

(defun gen-label (&optional (label 'L))
  (intern (format nil "~a~d" label (incf *label-num*))))

(defun gen-set (var env)
  (multiple-value-bind (i j)(in-environment? var env)
    (if i
        (gen 'LSET i j ";" var)
        (gen 'GSET var))))

(defun gen-var (var env)
  (multiple-value-bind (i j)(in-environment? var env)
    (if i
        (gen 'LREF i j ";" var)
        (gen 'GREF var))))

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

(defun comp-begin (exps env val? more?)
  (cond ((null exps) (comp-const (nothing) val? more?))
        ((length=1? exps) (comp (first exps) env val? more?))
        (t (seq (comp (first exps) env t t)
                `((POP))
                (comp-begin (rest exps) env val? more?)))))

(defun comp-const (x val? more?)
  (if val?
      (seq (cond
             ((equal x *undefined*)(gen 'UNDEFINED))
             ((equal x *nothing*)(gen 'NOTHING))
             ((equal x *false*)(gen 'FALSE))
             ((equal x *true*)(gen 'TRUE))
             (t (gen 'CONST x)))
           (unless more? (gen 'RETURN)))
      nil))

(defun comp-if (pred then else env val? more?)
  (let ((pcode (comp pred env t t))
        (tcode (comp then env val? more?))
        (ecode (comp else env val? more?)))
    (let ((L1 (gen-label))
          (L2 (if more? (gen-label))))
      (seq pcode (gen 'FGO L1) tcode
           (if more? (gen 'GO L2))
           (list L1) ecode (if more? (list L2))))))

(defun comp-funcall (f args env val? more?)
  (let ((prim (primitive? f env (length args))))
    (cond
      (prim  ; function compilable to a primitive instruction
       (seq (comp-list args env)
            (gen (prim-opcode prim))
            (unless val? (gen 'POP))
            (unless more? (gen 'RETURN))))
      (more? ; Need to save the continuation point
       (let ((k (gen-label 'k)))
         (seq (gen 'SAVE k)
              (comp-list args env)
              (comp f env t t)
              (gen 'CALLJ (length args))
              (list k)
              (if (not val?) (gen 'POP)))))
      (t (seq (comp-list args env)
              (comp f env t t)
              (gen 'CALLJ (length args)))))))

(defun comp-list (exps env)
  (if (null exps) nil
      (seq (comp (first exps) env t t)
           (comp-list (rest exps) env))))

(defun comp-method (args body env)
  (let* ((params (make-true-list args))
         (call-env (cons params env)))
    (make-instance '<mfn> 
                   :expression (cons 'bard-symbols::|^| (cons args body))
                   :env env :args args
                   :code (assemble
                          (seq (gen-args args 0)
                               (comp-begin body call-env t nil))))))

(defun comp-set! (var-form val-form env val? more?)
  (assert (symbolp var-form) (expr)
          "Only variables can be set!, not ~a" var-form)
  (seq (comp val-form env t t)
       (gen-set var-form env)
       (if (not val?) (gen 'POP))
       (unless more? (gen 'RETURN))))

(defun comp-time (exp env val? more?)
  (seq (gen 'START-TIMER)
       (comp exp env val? t)
       (gen 'REPORT-TIME)
       (unless val? (gen 'POP))
       (unless more? (gen 'RETURN))))

(defun comp-var (x env val? more?)
  (if val?
      (seq (gen-var x env)
           (if more?
               nil
               (gen 'RETURN)))
      nil))

;;; ---------------------------------------------------------------------
;;; compiler utils
;;; ---------------------------------------------------------------------

(defun arg-count (form min &optional (max min))
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
      "Wrong number of arguments for ~a in ~a: 
       ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))

;;; ---------------------------------------------------------------------
;;; main compiler entry point
;;; ---------------------------------------------------------------------

(defun comp (expr env val? more?)
  (cond
    ;; named constants
    ((member expr `(,*eof* ,*undefined* ,*nothing* ,*false* ,*true*))
     (comp-const expr val? more?))

    ((keywordp expr) (comp-const expr val? more?))

    ;; variable references
    ((symbolp expr) (comp-var expr env val? more?))

    ;; self-evaluating values
    ((atom expr) (comp-const expr val? more?))

    ;; macro forms
    ((bard-macro? (first expr)) (comp (bard-macroexpand expr) env val? more?))

    ;; procedure applications
    (t (let* ((op (first expr)))
         (case op

           ;; special forms
           ;; ------------------
           (bard-symbols::|quote|
                          (arg-count expr 1)
                          (comp-const (second expr) val? more?))

           (bard-symbols::|begin|
                          (comp-begin (rest expr) env val? more?))

           (bard-symbols::|time|
                          (comp-time (second expr) env val? more?))

           (bard-symbols::|set!|
                          (arg-count expr 2)
                          (comp-set! (second expr)(third expr) env val? more?))

           (bard-symbols::|if|
                          (arg-count expr 3)
                          (comp-if (second expr) (third expr) (fourth expr)
                                   env val? more?))

           ((bard-symbols::|method| bard-symbols::|^|)
            (when val?
              (let ((f (comp-method (second expr) (drop 2 expr) env)))
                (seq (gen 'MFN f) (unless more? (gen 'RETURN))))))

           ;; function and method calls
           ;; --------------------------
           (t (comp-funcall (first expr) (rest expr) env val? more?)))))))

;;; ---------------------------------------------------------------------
;;; compiler for use from Bard code
;;; ---------------------------------------------------------------------

(defun compiler (x)
  (setf *label-num* 0)
  (comp-method '() (list x) (null-environment)))
