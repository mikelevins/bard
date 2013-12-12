;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; comp1
;;; ---------------------------------------------------------------------
;;; the first pass: expand all macros and reader macros

;;; comp1

(defparameter $named-constants '(bard::|nothing| bard::|undefined| bard::|true| bard::|false| bard::|eof|))

(defun named-constant? (exp &optional (env nil))
  (member exp $named-constants))

(defun comp1-named-constant (exp &optional (env nil))
  (ecase exp
    (bard::|nothing| '(%nothing%))
    (bard::|undefined| '(%undefined%))
    (bard::|true| '(%true%))
    (bard::|false| '(%false%))
    (bard::|eof| '(%eof%))))

(defun self-evaluating? (exp &optional (env nil))
  (or (null exp)
   (and (atom exp)
        (not (named-constant? exp))
        (not (symbolp exp)))))

(defmethod comp1-self-evaluating (exp &optional (env nil))
  exp)

(defmethod comp1-self-evaluating ((exp null) &optional (env nil))
  '(%nothing%))

(defmethod comp1-self-evaluating ((exp %undefined%) &optional (env nil))
  '(%undefined%))

(defmethod comp1-self-evaluating ((exp %true%) &optional (env nil))
  '(%true%))

(defmethod comp1-self-evaluating ((exp %false%) &optional (env nil))
  '(%false%))

(defmethod comp1-self-evaluating ((exp %eof%) &optional (env nil))
  '(%eof%))

(defun find-in-env? (exp env)
  (assoc exp env))

(defun comp1-variable-reference (exp &optional (env nil))
  (if (find-in-env? exp env)
      exp
      `(gref ,exp)))

(defun setter-form? (exp)
  (and (listp exp)
       (cdr exp)
       (eql 'bard::|setter| (car exp))))

(defun comp1-setter-form (exp &optional (env nil))
  (assert (and (cdr exp)(not (cddr exp)))()
          "Malformed setter expression: ~s" exp)
  `(setter ,(second exp)))

(defun comp1-define-method-parameters (argforms env)
  (if (null argforms)
      (values nil nil)
      (let ((argform (car argforms)))
        (multiple-value-bind (params types)(comp1-define-method-parameters (cdr argforms) env)
          (if (listp argform)
              (values (cons (car argform) params)
                      (cons (comp1 (cdr argform) env) types))
              (values (cons argform params)
                      (cons Anything types)))))))

(defun make-method-environment (formals env)
  (append (mapcar (lambda (f)(cons f nil)) formals)
          env))

(defun comp1-define-method (exp env)
  (let* ((proto (third exp))
         (fname (first proto)))
    (multiple-value-bind (formals signature)(comp1-define-method-parameters (cdr proto) env)
      (let* ((env* (make-method-environment formals env))
             (body (comp1 (cons 'bard::|begin| (nthcdr 3 exp)) env*)))
        `(define-bard-method ,fname (list ,@signature)
           (lambda ,formals ,body))))))

(defun comp1-definition (exp &optional (env nil))
  (assert (cdr exp)() "Malformed definition: ~s" exp)
  (case (second exp)
    (bard::|method| (comp1-define-method exp env))
    (t (error "Unrecognized definition type: ~s" (second exp)))))

(defparameter $special-forms (make-hash-table))

(defun defspecial (name compiler-fn)
  (setf (gethash name $special-forms) compiler-fn))

(defspecial 'bard::|begin|
  (lambda (exp env)
    `(progn ,@(mapcar (lambda (e)(comp1 e env))
                      (cdr exp)))))


(defspecial 'bard::|define|
  (lambda (exp env)
    (comp1-definition exp env)))

(defspecial 'bard::|if|
  (lambda (exp env)
    (assert (= 4 (length exp))() "Malformed if expression: ~s" exp)
    (let ((test (second exp))
          (then (third exp))
          (else (fourth exp)))
      `(if ,(comp1 test env)
           ,(comp1 then env)
           ,(comp1 else env)))))

(defspecial 'bard::|quote|
  (lambda (exp env)
    `(quote ,(second exp))))

(defun special-form? (exp)
  (gethash exp $special-forms nil))

(defun comp1-special-form (exp &optional (env nil))
  (funcall (gethash (car exp) $special-forms nil)
           exp env))

(defun macro-form? (exp)
  nil)

(defun expand-bard-macro (exp)
  nil)

(defun comp1-funcall (exp &optional (env nil))
  (let ((op (car exp))
        (args (cdr exp)))
    `(bard-funcall ,(comp1 op env)
                   ,@(mapcar (lambda (arg)(comp1 arg env))
                             args))))

(defun comp1 (exp &optional (env nil)) 
  (cond
    ((named-constant? exp) (comp1-named-constant exp env))
    ((self-evaluating? exp) (comp1-self-evaluating exp env))
    ((symbolp exp) (comp1-variable-reference exp env))
    ((listp exp) (cond
                   ((setter-form? exp)(comp1-setter-form exp env))
                   ((special-form? (car exp))(comp1-special-form exp env))
                   ((macro-form? exp) (comp1 (expand-bard-macro exp) env))
                   (t (comp1-funcall exp env))))
    (t (error "Syntax error: ~s" exp))))



;;; ---------------------------------------------------------------------
;;; file compilation
;;; ---------------------------------------------------------------------

(defmethod comp1-file ((in stream)(out stream))
  (let* ((eof (gensym)))
    (loop for obj = (bard-read in eof)
       until (eql obj eof)
       do (let ((*package* (find-package :keyword)))
            (write (comp1 obj) :stream out :escape t :readably t)
            (terpri out)))
    (finish-output out)))

(defmethod comp1-file ((path pathname) out)
  (with-open-file (in path :direction :input)
    (comp1-file in out)))

(defmethod comp1-file (in (path pathname))
  (with-open-file (out path :direction :output)
    (comp1-file in out)))

(defmethod comp1-file ((path string) out)
  (comp1-file (pathname path) out))

(defmethod comp1-file (in (path string))
  (comp1-file in (pathname path)))

;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/namer.bard" "/Users/mikel/Workshop/bard/0.5/testdata/namer.bardo")
;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/literals.bard" "/Users/mikel/Workshop/bard/0.5/testdata/literals.bardo")
;;; (comp1-file "/Users/mikel/Workshop/bard/0.5/testdata/programs.bard" "/Users/mikel/Workshop/bard/0.5/testdata/programs.bardo")
