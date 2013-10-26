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

(defmethod valid-module-name? ((x symbol))
  (let ((s (symbol-name x)))
    (and (alpha-char-p (elt s 0))
         (every (lambda (c) 
                  (or (char= c #\.)
                      (alphanumericp c))) 
                s))))

(deftype module-name ()
  `(and symbol
        (satisfies valid-module-name?)))

;;; ---------------------------------------------------------------------
;;; module variables
;;; ---------------------------------------------------------------------

(defclass <mvar> ()
  ((name :reader name :initarg :name)
   (mutable? :reader mutable? :initarg :mutable)
   (import-from :reader import-from :initform nil :initarg :import-from)
   (id :reader id :initarg :id)))

;;; ---------------------------------------------------------------------
;;; modules
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((name :reader module-name :initarg :name)
   (variables :accessor variables :initform (make-hash-table))))

(defun make-module (mname)
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (make-instance '<module> :name mname))

(defmethod add-module-variable! ((mname symbol)(vname symbol) &key (val *undefined*)(mutable t))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((module (find-module mname)))
    (if module
        (let ((var (gethash vname (variables module))))
          (assert (not var)() "Variable exists: ~s" vname)
          (let ((id (add-global! val)))
            (assert id () "Error defining a module variable: ~s" vname)
            (let ((mvar (make-instance '<mvar>
                                       :name vname
                                       :mutable mutable
                                       :id id)))
              (set-global! id val)
              val)))
        (error "No such module: ~s" mname))))

(defmethod get-module-variable ((mname symbol)(vname symbol))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((module (find-module mname)))
    (if module
        (let ((var (gethash vname (variables module))))
          (assert var () "Undefined module variable: ~a:~a" mname vname)
          (let ((id (id var)))
            (get-global id)))
        (error "No such module: ~s" mname))))

(defmethod set-module-variable! ((mname symbol)(vname symbol) val)
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((module (find-module mname)))
    (if module
        (let ((var (gethash vname (variables module))))
          (assert var () "Undefined module variable: ~a:~a" mname vname)
          (let ((id (id var)))
            (set-global! id val)
            val))
        (error "No such module: ~s" mname))))

;;; ---------------------------------------------------------------------
;;; module registry and current module
;;; ---------------------------------------------------------------------

(defparameter *module* (intern "bard.user" (find-package :bard-modules)))

(defparameter *module-registry* (make-hash-table))

(defmethod register-module! ((mname symbol))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((module (make-module mname)))
    (setf (gethash mname *module-registry*)
          module)))

(defmethod find-module ((mname symbol))
  (gethash mname *module-registry*))

(defun set-current-module! (mname)
  (assert (typep mname 'module-name)()
          "Not a valid module name: ~s" mname)
  (assert (find-module mname)()
          "No such module: ~s" mname)
  (setf *module* mname))

(defun get-current-module ()
  *module*)

;;; ---------------------------------------------------------------------
;;; initializing standard modules
;;; ---------------------------------------------------------------------

(defun init-standard-modules ()
  (register-module! 'bard-modules::bard.lang)
  (register-module! 'bard-modules::bard.user))
