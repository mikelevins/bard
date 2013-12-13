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

(defparameter $named-constants
  '(bard-symbols::|nothing| bard-symbols::|undefined| bard-symbols::|true| bard-symbols::|false| bard-symbols::|eof|))

(defun named-constant? (exp &optional (env nil))
  (member exp $named-constants))

(defun comp1-named-constant (exp &optional (env nil))
  (ecase exp
    (bard-symbols::|nothing| '(%nothing%))
    (bard-symbols::|undefined| '(%undefined%))
    (bard-symbols::|true| '(%true%))
    (bard-symbols::|false| '(%false%))
    (bard-symbols::|eof| '(%eof%))))

(defun self-evaluating? (exp &optional (env nil))
  (or (null exp)
   (and (atom exp)
        (not (named-constant? exp))
        (or (keywordp exp)
            (not (symbolp exp))))))

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
       (eql 'bard-symbols::|setter| (car exp))))

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

;;; (define class Point [] 
;;;   point-x (-> Point -> Number)
;;;   point-y (-> Point -> Number))
;;; (define class Point3D [Point]
;;;   point-z (-> Point -> Number))
(defun comp1-define-class (exp env)
  (let* ((class-name (third exp))
         (superclass-list (fourth exp))
         (superclasses (mapcar (lambda (c)(comp1 c env))
                               superclass-list))
         (protocol-alist (mapcar (lambda (fn) 
                                   (let ((fname (car fn))
                                         (fn (cdr fn)))
                                     (cons fname (comp1 fn env))))
                                 (plist->alist (drop 4 exp))))
         (protocol (alist->plist protocol-alist)))
    `(define-bard-class ,class-name ,superclasses ,protocol)))

(defun comp1-define-constant (exp env)
  (let* ((cname (third exp))
         (cval (fourth exp)))
    `(define-bard-constant ,cname ,(comp1 cval env))))

(defun comp1-define-variable (exp env)
  (let* ((vname (third exp))
         (vval (fourth exp)))
    `(define-bard-variable ,vname ,(comp1 vval env))))

(defun comp1-define-method (exp env)
  (let* ((proto (third exp))
         (fname (first proto)))
    (multiple-value-bind (formals signature)(comp1-define-method-parameters (cdr proto) env)
      (let* ((env* (make-method-environment formals env))
             (body (comp1 (cons 'bard-symbols::|begin| (drop 3 exp)) env*)))
        `(define-bard-method ,fname (list ,@signature)
           (lambda ,formals ,body))))))

(defun comp1-definition (exp &optional (env nil))
  (assert (cdr exp)() "Malformed definition: ~s" exp)
  (case (second exp)
    (bard-symbols::|class| (comp1-define-class exp env))
    (bard-symbols::|constant| (comp1-define-constant exp env))
    (bard-symbols::|method| (comp1-define-method exp env))
    (bard-symbols::|variable| (comp1-define-variable exp env))
    (t (error "Unrecognized definition type: ~s" (second exp)))))

;;; (-> Integer Integer List -> List)
(defun comp1-function (exp env)
  (let* ((form (rest exp))
         (arrow-pos (position 'bard-symbols::|->| form)))
    (if arrow-pos
        (let* ((input-list (subseq form 0 arrow-pos))
               (inputs (mapcar (lambda (i)(comp1 i env))
                               input-list))
               (output-list (subseq form (1+ arrow-pos)))
               (outputs (mapcar (lambda (o)(comp1 o env))
                                output-list)))
          `(bard-function (list ,@inputs) (list ,@outputs)))
        (error "Malformed literal function expression: ~s" exp))))

(defun symbol-restarg? (thing)
  (eql thing 'bard-symbols::&))

(defun map-restarg? (thing)
  (and thing
       (listp thing)
       (eql 'bard-symbols::|map|
            (first thing))))

(defun restarg? (thing)
  (or (symbol-restarg? thing)
      (map-restarg? thing)))

(defun restpos (param-list)
  (position-if 'restarg? param-list))

(defun restarg (param-list)
  (let* ((restpos (restpos param-list)))
    (if restpos
        (elt param-list restpos)
        nil)))

(defun comp1-method-simple-param-list (exp env)
  (let* ((param-list (second exp))
         (env* (append (mapcar (lambda (p)(cons p nil))
                               param-list)
                       env))
         (body (comp1 (cons 'bard-symbols::|begin| (drop 2 exp))
                      env*)))
    `(make-bard-method ,param-list ,nil ,body ,env*)))

(defun comp1-method-symbol-restarg (exp env)
  (let* ((param-list (second exp))
         (restpos (restpos param-list))
         (required-args (take restpos param-list))
         (restarg (elt param-list (1+ restpos))))
    (assert (symbolp restarg)() "Invalid rest argument: ~s" restarg)
    (let* ((env* (append (mapcar (lambda (p)(cons p nil))
                                 (cons restarg required-args))
                         env))
           (body (comp1 (cons 'bard-symbols::|begin| (drop 2 exp))
                        env*)))
      `(make-bard-method ,required-args ,restarg ,body ,env*))))

(defun comp1-method-map-restarg (exp env)
  (let* ((param-list (second exp))
         (restpos (restpos param-list))
         (required-args (take restpos param-list))
         (restarg (elt param-list restpos)))
    (assert (null (drop (1+ restpos) param-list))() 
            "Malformed parameter list; arguments appear after the map argument: ~s"
            param-list)
    (let* ((rest-plist (drop 1 restarg))
           (rest-alist (plist->alist rest-plist))
           (rest-keys (mapcar 'car rest-alist))
           (rest-vals (mapcar (lambda (p)(comp1 (cdr p) env))
                              rest-alist))
           (rest-alist* (mapcar 'cons rest-keys rest-vals))
           ;; make a compilation environment in which the map's keys are the names of lexical variables
           (env* (append (mapcar (lambda (p)(cons (intern (symbol-name (car p)) :bard-symbols)
                                                  (cdr p)))
                                 rest-alist*)
                         (mapcar (lambda (p)(cons p nil)) required-args)
                         env))
           (restarg `(bard::bard-funcall (bard::gref bard-symbols::|map|)
                                         ,@(alist->plist rest-alist*)))
           (body (comp1 (cons 'bard-symbols::|begin| (drop 2 exp))
                        env*)))
      `(make-bard-method ,required-args ,restarg ,body ,env*))))

(defun comp1-method (exp env)
  (let* ((param-list (second exp))
         (restarg (restarg param-list)))
    (cond
      ((symbol-restarg? restarg)(comp1-method-symbol-restarg exp env))
      ((map-restarg? restarg)(comp1-method-map-restarg exp env))
      (t (comp1-method-simple-param-list exp env)))))

(defparameter $special-forms (make-hash-table))

(defun defspecial (name compiler-fn)
  (setf (gethash name $special-forms) compiler-fn))

(defspecial 'bard-symbols::|begin|
  (lambda (exp env)
    `(progn ,@(mapcar (lambda (e)(comp1 e env))
                      (cdr exp)))))

(defspecial 'bard-symbols::|define|
  (lambda (exp env)
    (comp1-definition exp env)))

(defspecial 'bard-symbols::|function|
  (lambda (exp env)(comp1-function exp env)))
(defspecial 'bard-symbols::|->|
  (lambda (exp env)(comp1-function exp env)))

(defspecial 'bard-symbols::|if|
  (lambda (exp env)
    (assert (= 4 (length exp))() "Malformed if expression: ~s" exp)
    (let ((test (second exp))
          (then (third exp))
          (else (fourth exp)))
      `(if ,(comp1 test env)
           ,(comp1 then env)
           ,(comp1 else env)))))

(defspecial 'bard-symbols::|method|
  (lambda (exp env)(comp1-method exp env)))

(defspecial 'bard-symbols::|quote|
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
  (loop for obj = (bard-read in (%eof))
     until (%eof? obj)
     do (let ((*package* (find-package :keyword)))
          (write (comp1 obj) :stream out :escape t :readably t)
          (terpri out)))
  (finish-output out))

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
