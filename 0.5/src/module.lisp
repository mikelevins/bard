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
;;; constructor helpers
;;; ---------------------------------------------------------------------

(defmethod valid-module-name-constituent? ((ch character))
  (find ch ".-abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890" :test #'char=))

(defmethod valid-module-name? (x) nil)

(defmethod valid-module-name? ((name string))
  (and (not (zerop (length name)))
       (alpha-char-p (elt name 0))
       (every #'valid-module-name-constituent? name)))

(defmethod ensure-valid-module-name ((name string))
  (or (and (valid-module-name? name)
           name)
      (error "Invalid module name: ~S" name)))

;;; ---------------------------------------------------------------------
;;; module class
;;; ---------------------------------------------------------------------

(defclass module ()
  ((name :accessor module-name :initarg :name)
   ;; symbol->name
   (symbols :accessor %symbols :initform (make-hash-table) :initarg :symbols)
   ;; name->symbol
   (names :accessor %names :initform (make-hash-table :test #'equal) :initarg :names)))

(defmethod print-object ((module module)(out stream))
  (princ "#<module " out)
  (princ (module-name module) out)
  (princ ">" out))

;;; ---------------------------------------------------------------------
;;; interning symbols
;;; ---------------------------------------------------------------------

(defmethod symbol-name ((symbol symbol) &optional (module nil))
  (let ((module (or module (symbol-module symbol))))
    (gethash symbol (%symbols module))))

(defmethod intern ((name string)(module module) &key (as nil))
  (let ((local-name (or as name)))
    (if (equal name local-name)
        (let ((sym (make-instance 'symbol :module module)))
          (setf (gethash sym (%symbols module)) local-name)
          (setf (gethash local-name (%names module)) sym)
          sym)
        (error "Tried to intern a string (~S) as a different string (~S)"
               name as))))

(defmethod intern ((name symbol)(module module) &key (as nil))
  (let* ((symbol-module (symbol-module name))
         (local-name (or as (symbol-name name))))
    (setf (gethash sym (%symbols module)) local-name)
    (setf (gethash local-name (%names module)) sym)
    sym))

(defmethod in-module? ((symbol symbol)(module module))
  (multiple-value-bind (val found?)(gethash symbol (%symbols module))
    (declare (ignore val))
    found?))

;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------

(defparameter *modules* (map))
(defparameter *module* |undefined|)

(defun current-module () *module*)

(defmethod register-module ((name string)(module module))
  (let ((mname (ensure-valid-module-name name)))
    (assert (equal mname (module-name module))() 
            "Cannot register module named ~S under a different name (~S)"
            (module-name module) mname)
    (setf *modules* (put-key *modules* name module))
    mname))

(defmethod find-module ((name string))
  (get-key *modules* name))

(defun init-modules ()
  (register-module "bard.base" (make-instance 'module :name (ensure-valid-module-name "bard.base")))
  (register-module "bard.user" (make-instance 'module :name (ensure-valid-module-name "bard.user")))
  (setf *module* (find-module "bard.user")))

;;; (init-modules)
;;; (intern "Foo" (current-module))
;;; (intern "Bar" (find-module "bard.base"))
