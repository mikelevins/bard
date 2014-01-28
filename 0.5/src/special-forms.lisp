;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special-forms.lisp
;;;; Project:       Bard
;;;; Purpose:       special-form definitions for compiler1
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************


(in-package :bard)


;;; ---------------------------------------------------------------------
;;; special form definitions
;;; ---------------------------------------------------------------------

;;; abort
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|abort|
      (lambda (expr env)
        (argument-count expr 1)
        (let* ((condition (compile (cadr expr) env)))
          `(:abort ,condition)))))

;;; and
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|and|
      (lambda (expr env)
        (let ((exprs (cdr expr)))
          (if (null exprs)
              (compile (true) env)
              (let ((test (compile (car exprs) env))
                    (more (cdr exprs)))
                (if more
                    `(:if ,test ,(compile (cons '|and| more) env) ,(compile (false) env))
                    test)))))))

;;; begin
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|begin|
      (lambda (expr env)
        (let* ((exprs (rest expr))
               (body (mapcar (lambda (ex)(compile ex env))
                             exprs)))
          `(:begin ,@body)))))

;;; case
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|case|
      (lambda (expr env)
        (let* ((case-val (compile (second expr) env))
               (clause-forms (nthcdr 2 expr))
               (clauses (mapcar (lambda (clause)
                                  (let* ((test (first clause))
                                         (body (rest clause)))
                                    (list test (compile (cons '|begin| body) env))))
                                clause-forms)))
          `(:case ,case-val ,@clauses)))))


;;; cond
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|cond|
      (lambda (expr env)
        (let* ((clause-forms (cdr expr))
               (clauses (mapcar (lambda (clause)
                                  (let* ((test (first clause))
                                         (body (rest clause)))
                                    (list (compile test env) 
                                          (compile (cons '|begin| body) env))))
                                clause-forms)))
          `(:cond ,@clauses)))))

;;; def
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|def|
      (lambda (expr env)
        (let* ((vname (second expr))
               (val (third expr)))
          `(:define-variable ,vname ,val)))))

;;; define
;;; ---------------------------------------------------------------------

;;; (define class Point ())
(defun compile-define-class (body env)
  (let ((cname (first body))
        (supers (second body)))
    `(:define-class ,cname ,supers)))

(defun compile-define-condition (body env)
  (let ((cname (first body))
        (supers (second body))
        (slots (third body)))
    `(:define-condition ,cname ,supers ,slots)))

(defun compile-define-constant (body env)
  `(:define-constant ,@body))

;;; (define enumeration color [:Red :Green :Blue])
(defun compile-define-enumeration (body env)
  (let ((ename (first body))
        (members (second body)))
    `(:define-enumeration ,ename ,members)))

(defun compile-define-macro (body env)
  (let ((mname (first body))
        (param-list (second body))
        (mbody (nthcdr 2 body)))
    `(:define-macro ,mname ,param-list (:begin ,@mbody))))

(defun lambda-list-formals (lambda-list)
  (mapcar (lambda (p)
            (cond
              ((symbolp p) p)
              ((consp p) 
               (assert (= 2 (length p))() "Invalid parameter: ~s" p)
               (if (symbolp (first p)) 
                             (first p)
                             (error "Invalid parameter name: ~s" (first p))))
              (t (error "Invalid parameter: ~s" p))))
          lambda-list))

(defun lambda-list-type-constraints (lambda-list)
  (mapcar (lambda (p)
            (cond
              ((symbolp p) '|Anything|)
              ((consp p) 
               (assert (= 2 (length p))() "Invalid parameter: ~s" p)
               (second p))
              (t (error "Invalid parameter: ~s" p))))
          lambda-list))

(defun compile-define-method (body env)
  (let* ((proto (first body))
         (fname (first proto))
         (formals (lambda-list-formals (second proto)))
         (signature (lambda-list-type-constraints (second proto)))
         (mbody (cdr body))
         (method (compile `(|method| ,formals ,@mbody) env)))
    `(:define-method ,fname ,signature ,method)))

(defun compile-define-protocol (body env)
  (let* ((pname (first body))
         (supers (second body))
         (funs (nthcdr 2 body)))
    `(:define-protocol ,pname ,supers ,funs)))

(defun compile-define-record (body env)
  (let* ((rname (first body))
         (slots (cdr body)))
    `(:define-record ,rname ,slots)))

(defun compile-define-setter (body env)
  (let* ((pattern (first body))
         (arg (second body))
         (sbody (nthcdr 2 body)))
    `(:define-setter ,pattern ,arg ,sbody)))

(defun compile-define-tuple (body env)
  (let* ((tname (first body))
         (rules (cdr body)))
    `(:define-tuple ,tname ,rules)))

(defun compile-define-union (body env)
  (let* ((uname (first body))
         (members (second body)))
    `(:define-union ,uname ,members)))

(defun compile-define-variable (body env)
  (let* ((vname (first body))
         (val (second body)))
    `(:define-variable ,vname ,val)))


(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|define|
      (lambda (expr env)
        (let* ((dtype (second expr))
               (body (nthcdr 2 expr)))
          (case dtype
            (|class| (compile-define-class body env))
            (|condition| (compile-define-condition body env))
            (|constant| (compile-define-constant body env))
            (|enumeration| (compile-define-enumeration body env))
            (|macro| (compile-define-macro body env))
            (|method| (compile-define-method body env))
            (|protocol| (compile-define-protocol body env))
            (|record| (compile-define-record body env))
            (|setter| (compile-define-setter body env))
            (|tuple| (compile-define-tuple body env))
            (|union| (compile-define-union body env))
            (|variable| (compile-define-variable body env))
            (t (error "Unknown definition type: ~S" dtype)))))))


;;; catch
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|catch|
      (lambda (expr env)
        (let ((tag (cadr expr))
              (body (compile (cons '|begin| (cddr expr)) env)))
          `(:catch ,tag ,body)))))

;;; ensure
;;; ---------------------------------------------------------------------

;;; (ensure x :after y)
(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|ensure|
      (lambda (expr env)
        (let* ((before-form (fourth expr))
               (after-form (second expr)))
          `(:unwind-protect ,before-form ,after-form)))))

;;; eval
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|eval|
      (lambda (expr env)
        `(:eval ,(second expr) ,env))))

;;; function
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|->|
      (lambda (expr env)
        (let* ((body (cdr expr))
               (arrow-pos (position '-> body)))
          (assert arrow-pos ()() "Invalid function syntax: ~S" expr)
          (let ((inputs (subseq body 0 arrow-pos))
                (outputs (subseq body (1+ arrow-pos))))
            `(:function ,inputs ,outputs))))))


;;; if
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|if|
      (lambda (expr env)
        (let ((test (second expr))
              (then (third expr))
              (else (fourth expr)))
          `(:if ,(compile test env)
                ,(compile then env)
                ,(compile else env))))))

;;; loop
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|loop|
      (lambda (expr env)
        (let* ((loopname (second expr))
               (binding-forms (third expr))
               (bindings (mapcar (lambda (f)(list (first f)
                                                  (compile (second f) env)))
                                 binding-forms))
               (call-env (extend-env env loopname (cons :loopfun (gensym "loop"))))
               (body (compile (cons '|begin| (nthcdr 3 expr)) call-env)))
          `(:loop ,loopname ,bindings ,body)))))

;;; method
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|method|
      (lambda (expr env)
        (let* ((formals (second expr))
               (formal-bindings (loop for f in formals append (list f (nothing))))
               (body (nthcdr 2 expr))
               (call-env (apply #'extend-env (cons env formal-bindings))))
          `(:method ,formals ,(compile (cons '|begin| body) call-env))))))

;;; quote
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|quote|
      (lambda (expr env)
        (argument-count expr 1)
        `(:constant ,(cadr expr)))))

;;; throw
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (define-special '|throw|
      (lambda (expr env)
        (let ((tag (cadr expr))
              (val (caddr expr)))
          `(:throw ,tag val)))))

