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

(defun current-module-name ()
  (intern "bard.user" (find-package :bard-modules)))

;;; ---------------------------------------------------------------------
;;; module variables
;;; ---------------------------------------------------------------------

(defclass <mvar> ()
  ((name :accessor name :initform nil)
   (id :accessor id :initform nil)
   (mutable? :accessor mutable? :initform nil)
   (exported? :accessor exported? :initform nil)
   (import-from :accessor import-from :initform nil)
   (import-name :accessor import-name :initform nil)))

(defun make-mvar (name id &key (mutable t) (exported nil) (import-from nil) (import-name nil))
  (make-instance '<mvar> name id 
                 :mutable mutable 
                 :exported exported
                 :import-from import-from
                 :import-name import-name))

;;; ---------------------------------------------------------------------
;;; module names
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((globals :accessor globals :initform nil :initarg :globals)
   (variables-by-name :accessor variables-by-name :initform (make-hash-table))
   (variables-by-id :accessor variables-by-id :initform (make-hash-table))))

(defun make-module (gs)(make-instance '<module> :globals gs))

(defmethod add-module-variable! ((module <module>)(id integer)(name symbol) 
                          &key (mutable t) (exported nil) (import-from nil) (import-name nil))
  (let ((id (ensure-variable-with-id (globals module) id))
        (mvar (make-mvar name id &key :mutable mutable :exported exported :import-from import-from :import-name import-name)))
    (setf (gethash name (variables-by-name module)) mvar)
    (setf (gethash id (variables-by-id module)) mvar)
    module))

(defmethod remove-module-variable! ((module <module>)(name symbol))
  (let ((mvar (gethash name (variables-by-name module)))
        (id (id mvar)))
    (remhash name (variables-by-name module))
    (remhash id (variables-by-id module))
    module))

(defmethod lookup-module-variable ((module <module>)(name symbol))
  (let ((mvar (gethash name (variables-by-name module)))
        (id (id mvar)))
    (get-global (globals module) id)))

;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------

(defparameter *module-registry* (make-hash-table))

(defmethod find-module ((mname symbol))
  (gethash mname *module-registry*))

;;; ---------------------------------------------------------------------
;;; initializing standard modules
;;; ---------------------------------------------------------------------

(defun init-standard-modules ()
  (let* ((globals (make-instance '<globals>))
         (lang-module (make-module globals))
         (user-module (make-module globals)))
    (setf (gethash 'bard-modules::bard.lang *module-registry*) lang-module)
    (setf (gethash 'bard-modules::bard.user *module-registry*) user-module)))
