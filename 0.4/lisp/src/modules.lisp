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
;;; modules
;;; ---------------------------------------------------------------------

(defclass <module> ()
  ((name :reader module-name :initarg :name)
   (variables :accessor variables :initform (make-hash-table))))

(defun make-module (mname)
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (make-instance '<module> :name mname))

(defmethod bard-intern ((str string)(mname string))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (assert (find-module mname) () "No such module: ~s" mname)
  (let ((m (find-module mname))
        (s (make-bard-symbol str)))
    (setf (gethash (name s) (variables m)) s)
    (setf (module s) mname)
    s))

(defmethod bard-intern ((s <symbol>)(mname string))
  (assert (typep mname 'module-name)() "Not a valid module name: ~s" mname)
  (let ((m (find-module mname)))
    (assert m () "No such module: ~s" mname)
    (setf (gethash (name s)(variables m)) s)
    (setf (module s) (name m))
    s))

(defmethod add-module-variable! ((var <symbol>) &key (value *undefined*)(mutable t))
  )

(defmethod get-module-variable ((var <symbol>))
  )

(defmethod set-module-variable! ((var <symbol>) val)
  )

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
