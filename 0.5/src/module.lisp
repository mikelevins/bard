;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          module.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard modules
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; module-construction helpers
;;; ---------------------------------------------------------------------

(defmethod module-name-constituent? ((ch character))
  (find ch ".-1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :test 'char=))

(defmethod valid-module-name? ((name string))
  (and (not (zerop (length name)))
       (alpha-char-p (elt name 0))
       (every #'module-name-constituent? name)))

(defmethod ensure-valid-module-name ((name string))
  (if (valid-module-name? name)
      name
      (error "Invalid module name: ~a" name)))

;;; ---------------------------------------------------------------------
;;; module class
;;; ---------------------------------------------------------------------

(defclass module ()
  ((name :accessor module-name :initarg :name)
   (symbols :accessor %symbols :initform (fset:map) :initarg :symbols)
   (imports :accessor %imports :initform (fset:map) :initarg :imports)
   (exports :accessor %exports :initform (fset:map) :initarg :exports)))

(defmethod print-object ((obj module)(out stream))
  (princ "#<module " out)
  (princ (module-name obj) out)
  (princ ">" out))

;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------

(defparameter *modules* (fset:map))

(defmethod register-module ((name string)(module module))
  (assert (equal (module-name module) name)() "The module's name is ~A, not ~A" (module-name module) name)
  (let ((mname (ensure-valid-module-name name)))
    (setf *modules* 
          (put-key *modules*
                   mname module))
    mname))

(defmethod find-module ((name string))
  (let ((mname (ensure-valid-module-name name)))
    (get-key *modules* mname)))

;;; (register-module "bard.test" (make-instance 'module :name "bard.test"))
;;; (find-module "bard.test")

;;; ---------------------------------------------------------------------
;;; symbol-handling
;;; ---------------------------------------------------------------------

(defmethod intern ((name string)(module module) &key (export nil))
  (let* ((already (get-key (%symbols module) name))
         (sym (or already (make-instance 'symbol :name name :module module))))
    (unless already
      (setf (%symbols module)
            (put-key (%symbols module)
                     name sym)))
    (when export
      (setf (%exports module)
            (put-key (%exports module)
                     name t)))
    sym))

;;; (intern "Foo" (find-module "bard.test"))

(defmethod find-symbol ((name string)(module module))
  (get-key (%symbols module) name :default nil))

;;; (find-symbol "Foo" $mod)

;;; ---------------------------------------------------------------------
;;; standard modules
;;; ---------------------------------------------------------------------

(defparameter *module* nil)

(defun current-module () *module*)
(defmethod set-current-module! ((m module)) 
  (setf *module* m))

(defun init-modules ()
  (register-module "bard.base" (make-instance 'module :name "bard.base"))
  (register-module "bard.user" (make-instance 'module :name "bard.user"))
  (setf *module* (find-module "bard.user")))
