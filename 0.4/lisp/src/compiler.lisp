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
;;; method-functions
;;; ---------------------------------------------------------------------

(defun print-mfn (mfn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (if (mfn-name mfn)
      (format stream "#<method-function>{name: ~a}" (mfn-name mfn))
      (format stream "#<method-function>")))

(defstruct (mfn (:print-function print-mfn))
  code (env nil) (name nil) (args nil))

(defun new-mfn (&key code env name args)
  (assemble (make-mfn :env env :name name :args args
                      :code (optimize code))))

;;; ---------------------------------------------------------------------
;;; code generators
;;; ---------------------------------------------------------------------

(defun seq (&rest code)
  (apply #'append code))

(defun gen (opcode &rest args)
  (list (cons opcode args)))

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

(defvar *label-num* 0)

(defun gen-label (&optional (label 'L))
  (intern (format nil "~a~d" label (incf *label-num*))))

(defun gen-args (args n-so-far)
  (cond ((null args) (gen 'ARGS n-so-far))
        ((symbolp args) (gen 'ARGS. n-so-far))
        ((and (consp args) (symbolp (first args)))
         (gen-args (rest args) (+ n-so-far 1)))
        (t (error "Illegal argument list"))))

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

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

(defun comp-var (x env val? more?)
  (if val?
      (seq (gen-var x env)
           (if more?
               nil
               (gen 'RETURN)))
      nil))

(defun comp-list (exps env)
  (if (null exps) nil
      (seq (comp (first exps) env t t)
           (comp-list (rest exps) env))))

(defun comp-begin (exps env val? more?)
  (cond ((null exps) (comp-const (nothing) val? more?))
        ((length=1? exps) (comp (first exps) env val? more?))
        (t (seq (comp (first exps) env t t)
                `((POP))
                (comp-begin (rest exps) env val? more?)))))

(defun comp-if (pred then else env val? more?)
  (let ((pcode (comp pred env t t))
        (tcode (comp then env val? more?))
        (ecode (comp else env val? more?)))
    (let ((L1 (gen-label))
          (L2 (if more? (gen-label))))
      (seq pcode (gen 'FGO L1) tcode
           (if more? (gen 'GO L2))
           (list L1) ecode (if more? (list L2))))))

(defun comp-method (args body env)
  (let* ((params (make-true-list args))
         (call-env (extend-environment params (times (length params) (undefined)) env)))
    (new-mfn :env env :args args
             :code (seq (gen-args args 0)
                        (comp-begin body call-env t nil)))))

(defun comp-funcall (f args env val? more?)
  (let ((prim (primitive? f env (length args))))
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
      ((and (starts-with? f '|method|) (null (second f)))
       ;; ((method () body)) => (begin body)
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
      (t (seq (comp-list args env)
              (comp f env t t)
              (gen 'CALLJ (length args)))))))

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
;;; compiler init
;;; ---------------------------------------------------------------------

(defun init-bard-comp ()
  (init-globals)
  (init-standard-modules)
  (let ((lang-module (find-module 'bard-modules::|bard.lang|)))
    (set-module-variable! lang-module 'bard::|exit|
                          (new-mfn :name '|exit| :args '(val) :code '((HALT))))
    (set-module-variable! lang-module 'bard::|call/cc|
                          (new-mfn :name '|call/cc| :args '(f) :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
                                                                       (CALLJ 1))))))

;;; ---------------------------------------------------------------------
;;; main compiler entry point
;;; ---------------------------------------------------------------------

(defun comp (expr env val? more?)
  (cond
    ;; simple constants
    ((member expr `(,*eof* ,*undefined* ,*nothing* ,*false* ,*true*))
     (comp-const expr val? more?))
    ;; variable references
    ((symbolp expr) (comp-var expr env val? more?))
    ;; other self-evaluating values
    ((atom expr) (comp-const expr val? more?))
    ;; macro forms
    ((bard-macro? (first expr)) (comp (bard-macroexpand expr) env val? more?))
    ;; procedure applications
    ((case (first expr)
       (|quote|  (arg-count expr 1)
                 (comp-const (second expr) val? more?))
       (|begin|  (comp-begin (rest expr) env val? more?))
       (|set!|   (arg-count expr 2)
                 (assert (symbolp (second expr)) (expr)
                         "Only variables can be set!, not ~a in ~a"
                         (second expr) expr)
                 (seq (comp (third expr) env t t)
                      (gen-set (second expr) env)
                      (if (not val?) (gen 'POP))
                      (unless more? (gen 'RETURN))))
       (|if|     (arg-count expr 3)
                 (comp-if (second expr) (third expr) (fourth expr)
                          env val? more?))
       ((|method| |^|) (when val?
                         (let ((f (comp-method (second expr) (rest2 expr) env)))
                           (seq (gen 'MFN f) (unless more? (gen 'RETURN))))))
       (t      (comp-funcall (first expr) (rest expr) env val? more?))))))
