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
;;; instruction utilities
;;; ---------------------------------------------------------------------

(defun arg-count (form min &optional (max min))
  "Report an error if form has wrong number of args."
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
      "Wrong number of arguments for ~a in ~a: ~d supplied, ~d~@[ to ~d~] expected"
      (first form) form n-args min (if (/= min max) max))))

;;; ---------------------------------------------------------------------
;;; macro support
;;; ---------------------------------------------------------------------

(defparameter *bard-macroexpanders* (make-hash-table))

(defmethod bard-macro? (x)
  (declare (ignore x))
  nil)

(defmethod bard-macro? ((x symbol))
  (gethash x *bard-macroexpanders*))

(defmacro def-bard-macro (name parmlist &body body)
  `(setf (gethash ',name *bard-macroexpanders*)
         #'(lambda ,parmlist .,body)))

(defun bard-macroexpand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro? (first x)))
      (bard-macroexpand
        (apply (bard-macro? (first x)) (rest x)))
      x))

;;; built-in macros
;;; ---------------------------------------------------------------------

(def-bard-macro |def| (name val-form)
  `(|set!| ,name ,val-form))

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

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

(defun comp-funcall (f args env val? more?)
  (declare (ignore f args env val? more?))
  (not-yet-implemented 'comp-funcall))

(defun comp-method (args body env)
  (declare (ignore args body env))
  (not-yet-implemented 'comp-method))

(defun comp-if (pred then else env val? more?)
  (declare (ignore pred then else env val? more?))
  (not-yet-implemented 'comp-if))

(defun comp-begin (exps env val? more?)
  (declare (ignore exps env val? more?))
  (not-yet-implemented 'comp-begin))

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

(defun comp-begin (exps env val? more?)
  (cond ((null exps) (comp-const (nothing) val? more?))
        ((length=1? exps) (comp (first exps) env val? more?))
        (t (seq (comp (first exps) env t t)
                `((POP))
                (comp-begin (rest exps) env val? more?)))))

(defun comp-if (pred then else env val? more?)
  (cond
    ((undefined? pred)    ; (if undefined x y) ==> x
     (comp then env val? more?))
    ((nothing? pred)      ; (if nothing x y) ==> y
     (comp else env val? more?))
    ((true? pred)         ; (if true x y) ==> y
     (comp then env val? more?))
    ((false? pred)        ; (if false x y) ==> y
     (comp else env val? more?))
    ((constantp pred)     ; (if t x y) ==> x
     (comp then env val? more?))
    (t (let ((pcode (comp pred env t t))
             (tcode (comp then env val? more?))
             (ecode (comp else env val? more?)))
         (cond
           ((equal tcode ecode) ; (if p x x) ==> (begin p x)
            (seq (comp pred env nil t) ecode))
           (t             ; (if p x y) ==> p (FJUMP L1) x L1: y
                          ; or p (FJUMP L1) x (JUMP L2) L1: y L2:
            (let ((L1 (gen-label))
                  (L2 (if more? (gen-label))))
              (seq pcode (gen 'FGO L1) tcode
                   (if more? (gen 'GO L2))
                   (list L1) ecode (if more? (list L2))))))))))

(defun comp-if (pred then else env val? more?)
  (let ((pcode (comp pred env t t))
        (tcode (comp then env val? more?))
        (ecode (comp else env val? more?)))
    (let ((L1 (gen-label))
          (L2 (if more? (gen-label))))
      (seq pcode (gen 'FGO L1) tcode
           (if more? (gen 'GO L2))
           (list L1) ecode (if more? (list L2))))))

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
                 (assert (symbolp (second expr)) (x)
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
                           (seq (gen 'FN f) (unless more? (gen 'RETURN))))))
       (t      (comp-funcall (first expr) (rest expr) env val? more?))))))
