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
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; Bard modules have different semantics from Common Lisp packages. Most
;;; noticeably, symbols imported in modules can be renamed, unlike symbols
;;; in CL packages.
;;;
;;; It's useful to reuse the CL package system because it provides an 
;;; efficient built-in way to map symbols to modules (by using package
;;; names as module names). That way, Bard can reuse all of the efficient
;;; symbol-handling infrastructure of Common Lisp, rather than reinventing
;;; it.
;;;
;;; However, we can;t simply use packages as modules, because of the
;;; differences in semantics. So Bard modules are distinct from Common
;;; Lisp packages, but, in order to leverage package names to identify
;;; modules, there is a Common Lisp package defined for each Bard
;;; module.  Symbols in a Bard module are interned in the Common Lisp
;;; Package of of the same name, enabling us to easily find a symbol's
;;; home module by examining its package.
;;;
;;; NOTE: this note was written just after implementing modules, and
;;; before implementing the corresponding package support.

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(defmethod module-name ((name symbol))
  (intern (symbol-name name) :bard))

(defmethod module-name ((name string))
  (intern name :bard))

(defmethod module-name-constituent? ((ch character))
  (find ch ".-1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" :test 'char=))

(defmethod valid-module-name? ((name string))
  (and (not (zerop (length name)))
       (alpha-char-p (elt name 0))
       (every 'module-name-constituent? name)))

(defmethod valid-module-name? ((name symbol))
  (valid-module-name? (symbol-name name)))

;;; ---------------------------------------------------------------------
;;; module class
;;; ---------------------------------------------------------------------

(defclass module ()
  ((name :accessor module-name :initarg :name)
   (package :accessor module-package :initarg :package)
   (renames :accessor module-renames :initform (fset:map) :initarg :renames)))

(defmethod make-module ((name string))
  (assert (valid-module-name? name)() "Invalid module name: ~a" name)
  (let* ((mname (module-name name))
         (package (or (find-package name)
                      (make-package name :use nil))))
    (make-instance 'module :name mname :package package)))

;;; ---------------------------------------------------------------------
;;; symbol api
;;; ---------------------------------------------------------------------

(defmethod intern ((name string)(module module))
  (let ((already (find-symbol name module)))
    (if already
        already
        (let ((package (module-package module)))
          (cl:intern name package)))))

(defmethod unintern ((name string)(module module))
  (let* ((rename-record (get (module-renames module) name)))
    (if rename-record
        (let ((orig-name (get rename-record :original-name))
              (orig-package (get rename-record :original-package)))
          (setf (module-renames module)
                (remove-key (module-renames module) name))
          (cl:unintern orig-name orig-package))
        (let ((package (module-package module)))
          (cl:unintern name package)))))

(defmethod find-symbol ((name string)(module module))
  (let* ((rename-record (get (module-renames module) name)))
    (if rename-record
        (let ((orig-name (get rename-record :original-name))
              (orig-package (get rename-record :original-package)))
          (cl:find-symbol orig-name orig-package))
        (let ((package (module-package module)))
          (cl:find-symbol name package)))))

(defmethod symbol-module ((sym symbol))
  (find-module (package-name (symbol-package sym))))

;;; ---------------------------------------------------------------------
;;; module registry
;;; ---------------------------------------------------------------------
;;; TODO: make registry operations thread-safe

(defparameter *modules* (fset:map))

(defmethod assert-module! ((name string)(module module))
  (setf *modules*
        (put-key *modules* (module-name name) module)))

(defmethod assert-module! ((name symbol)(module module))
  (assert-module! (symbol-name name) module))

(defmethod retract-module! ((name string))
  (setf *modules*
        (remove-key *modules* (module-name name))))

(defmethod retract-module! ((name symbol))
  (retract-module! (symbol-name name)))

(defmethod find-module ((name string))
  (get-key *modules* (module-name name)))

;;; ---------------------------------------------------------------------
;;; bard modules
;;; ---------------------------------------------------------------------

(assert-module! "bard.base" (make-module "bard.base"))
(assert-module! "bard.user" (make-module "bard.user"))

(defparameter *module* (find-module "bard.user"))
