;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard
;;;; Purpose:       bard modules
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; module names
;;; ---------------------------------------------------------------------

(defmethod valid-module-name? (x)(declare (ignore x)) nil)

(defmethod valid-module-name? ((s string))
  (and (alpha-char-p (elt s 0))
       (every (lambda (c) 
                (or (char= c #\.)
                    (alphanumericp c))) 
              s)))

(deftype module-name ()
  `(and string
        (satisfies valid-module-name?)))

;;; ---------------------------------------------------------------------
;;; module variables
;;; ---------------------------------------------------------------------

(defclass <variable> ()
  ((name :accessor name :initarg :name)
   (module :accessor module :initarg :module)
   (value :accessor value :initarg :value :initform *undefined*)
   (mutable? :accessor mutable? :initarg :mutable :initform nil)
   (exported? :accessor exported? :initarg :exported :initform nil)
   (import-name :accessor import-name :initarg :import-name :initform nil)
   (import-module :accessor import-module :initarg :import-module :initform nil)))

;;; ---------------------------------------------------------------------
;;; modules
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((name :reader module-name :initarg :name)
   (symbols :accessor symbols :initform (make-hash-table :test 'equal))
   (variables :accessor variables :initform (make-hash-table :test 'equal))))

(defun make-module (mname)
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (make-instance '<module> :name mname))

(defmethod bard-intern ((str string)(mname string))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (assert (find-module mname) () "No such module: ~s" mname)
  (let* ((m (find-module mname))
         (s (or (gethash str (symbols m))
                (make-bard-symbol str))))
    (setf (module s) mname)
    (setf (gethash str (symbols m)) s)
    s))

(defmethod add-module-variable! ((varname string)(mname string) &key (value *undefined*)(mutable t))
  (let* ((s (bard-intern varname mname))
         (m (find-module mname))
         (v (make-instance '<variable> :name varname :module mname :value value :mutable mutable)))
    (setf (gethash varname (variables m)) v)
    varname))

(defmethod get-module-variable ((sym <symbol>))
  (let* ((m (find-module (module sym)))
         (s (name sym))
         (v (gethash s (variables m))))
    (assert v () "Undefined variable ~a:~a" (module sym) s)
    (value v)))

(defmethod set-module-variable! ((sym <symbol>) val)
  (let* ((m (find-module (module sym)))
         (s (name sym))
         (v (gethash s (variables m))))
    (assert v () "Undefined variable ~a:~a" (module sym) s)
    (assert (mutable? v) () "Immutable variable ~a:~a" (module sym) s)
    (setf (value v) val)
    val))

;;; ---------------------------------------------------------------------
;;; module registry and current module
;;; ---------------------------------------------------------------------

(defparameter *module* "bard.user")

(defparameter *module-registry* (make-hash-table :test 'equal))

(defmethod register-module! ((mname string))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((module (make-module mname)))
    (setf (gethash mname *module-registry*)
          module)))

(defmethod find-module ((mname string))
  (gethash mname *module-registry*))

(defun set-current-module! (mname)
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (assert (find-module mname)() "No such module: ~s" mname)
  (setf *module* mname))

(defun get-current-module ()
  *module*)

;;; ---------------------------------------------------------------------
;;; initializing standard modules
;;; ---------------------------------------------------------------------

(defun init-standard-modules ()
  (register-module! "bard.base")
  (register-module! "bard.user"))

(defun list-modules ()
  (let ((result nil))
    (maphash (lambda (k v) (push k result)) 
             *module-registry*)
    (sort result (lambda (x y)(string< x y)))))
