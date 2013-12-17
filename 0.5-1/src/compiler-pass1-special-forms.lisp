;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-special-forms.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling special forms
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; the special-form registry
;;; ---------------------------------------------------------------------

(defparameter $special-forms (make-hash-table))

(defun defspecial (name compiler-fn)
  (setf (gethash name $special-forms) compiler-fn))

(defun special-form? (exp)
  (gethash exp $special-forms nil))

;;; ---------------------------------------------------------------------
;;; auxiliary compiler functions
;;; ---------------------------------------------------------------------

(defun comp1-special-form (exp &optional (env nil))
  (funcall (gethash (car exp) $special-forms nil)
           exp env))

;;; definitions
;;; ---------------------------------------------------------------------

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
    (bard-symbols::|condition| (warn "define condition is not yet implemented"))
    (bard-symbols::|constant| (comp1-define-constant exp env))
    (bard-symbols::|macro| (warn "define macro is not yet implemented"))
    (bard-symbols::|method| (comp1-define-method exp env))
    (bard-symbols::|record| (warn "define record is not yet implemented"))
    (bard-symbols::|setter| (warn "define setter is not yet implemented"))
    (bard-symbols::|tuple| (warn "define tuple is not yet implemented"))
    (bard-symbols::|union| (warn "define union is not yet implemented"))
    (bard-symbols::|variable| (comp1-define-variable exp env))
    (t (error "Unrecognized definition type: ~s" (second exp)))))

;;; functions
;;; ---------------------------------------------------------------------

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

;;; methods
;;; ---------------------------------------------------------------------

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

;;; let
;;; ---------------------------------------------------------------------

;;; TODO: this compiler produces code in the right general form, but I
;;; need to alter it to compile the body and the value forms in the
;;; bindings
(defun comp1-let (exp env)
  `(bard-let ,@(cdr exp)))

;;; ---------------------------------------------------------------------
;;; the special forms
;;; ---------------------------------------------------------------------

(defspecial 'bard-symbols::|and|
  (lambda (exp env)
    `(warn "and is not yet implemented")))

(defspecial 'bard-symbols::|begin|
  (lambda (exp env)
    `(progn ,@(mapcar (lambda (e)(comp1 e env))
                      (cdr exp)))))

(defspecial 'bard-symbols::|case|
  (lambda (exp env)
    `(warn "case is not yet implemented")))

(defspecial 'bard-symbols::|catch|
  (lambda (exp env)
    `(warn "catch is not yet implemented")))

(defspecial 'bard-symbols::|cond|
  (lambda (exp env)
    `(warn "cond is not yet implemented")))

(defspecial 'bard-symbols::|define|
  (lambda (exp env)
    (comp1-definition exp env)))

(defspecial 'bard-symbols::|do|
  (lambda (exp env)
    `(warn "do is not yet implemented")))

(defspecial 'bard-symbols::|dolist|
  (lambda (exp env)
    `(warn "dolist is not yet implemented")))

(defspecial 'bard-symbols::|dotimes|
  (lambda (exp env)
    `(warn "dotimes is not yet implemented")))

(defspecial 'bard-symbols::|ensure|
  (lambda (exp env)
    `(warn "ensure is not yet implemented")))

(defspecial 'bard-symbols::|function|
  (lambda (exp env)(comp1-function exp env)))

(defspecial 'bard-symbols::|->|
  (lambda (exp env)(comp1-function exp env)))

(defspecial 'bard-symbols::|handler-bind|
  (lambda (exp env)
    `(warn "handler-bind is not yet implemented")))

(defspecial 'bard-symbols::|handler-case|
  (lambda (exp env)
    `(warn "handler-case is not yet implemented")))

(defspecial 'bard-symbols::|if|
  (lambda (exp env)
    (assert (= 4 (length exp))() "Malformed if expression: ~s" exp)
    (let ((test (second exp))
          (then (third exp))
          (else (fourth exp)))
      `(if ,(comp1 test env)
           ,(comp1 then env)
           ,(comp1 else env)))))

(defspecial 'bard-symbols::|invoke-restart|
  (lambda (exp env)
    `(warn "invoke-restart is not yet implemented")))

(defspecial 'bard-symbols::|let|
  (lambda (exp env)(comp1-let exp env)))

(defspecial 'bard-symbols::|loop|
  (lambda (exp env)
    `(warn "loop is not yet implemented")))

(defspecial 'bard-symbols::|match|
  (lambda (exp env)
    `(warn "match is not yet implemented")))

(defspecial 'bard-symbols::|method|
  (lambda (exp env)(comp1-method exp env)))

(defspecial 'bard-symbols::|next-method|
  (lambda (exp env)
    `(warn "next-method is not yet implemented")))

(defspecial 'bard-symbols::|or|
  (lambda (exp env)
    `(warn "or is not yet implemented")))

(defspecial 'bard-symbols::|quote|
  (lambda (exp env)
    `(quote ,(second exp))))

(defspecial 'bard-symbols::|restart-bind|
  (lambda (exp env)
    `(warn "restart-bind is not yet implemented")))

(defspecial 'bard-symbols::|restart-case|
  (lambda (exp env)
    `(warn "restart-case is not yet implemented")))

(defspecial 'bard-symbols::|signal|
  (lambda (exp env)
    `(warn "signal is not yet implemented")))

(defspecial 'bard-symbols::|throw|
  (lambda (exp env)
    `(warn "throw is not yet implemented")))

(defspecial 'bard-symbols::|unless|
  (lambda (exp env)
    `(warn "unless is not yet implemented")))

(defspecial 'bard-symbols::|values|
  (lambda (exp env)
    `(warn "values is not yet implemented")))

(defspecial 'bard-symbols::|when|
  (lambda (exp env)
    `(warn "when is not yet implemented")))


