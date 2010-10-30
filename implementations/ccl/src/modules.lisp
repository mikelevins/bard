;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          modules.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       modules map names to variables
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; CLASS: module
;;; ========================================================================

(defclass module ()((entries :reader entries :initform (make-hash-table :test 'eql))))

(defmethod intern ((nm cl:string)(md module))
  (let ((interned-name (intern-for-name nm)))
    (or (gethash interned-name (entries md))
        (setf (gethash interned-name (entries md))
              (make-instance 'mutable-cell)))
    interned-name))

(defmethod intern ((nm cl:symbol)(md module))
  (intern (symbol-name nm) md))

;;; ------------------------------------------------
(let ((modules (make-hash-table)))
  (defmethod define-module ((module-name cl:string))
    (let ((mname (name module-name)))
      (setf (gethash mname modules)
            (make-instance 'module))
      mname))

  (defmethod define-module ((module-name cl:symbol))
    (define-module (symbol-name module-name)))
  
  (defmethod find-module ((module-name cl:string))
    (gethash (name module-name) modules nil))

  (defmethod find-module ((module-name cl:symbol))
    (find-module (symbol-name module-name))))
;;; ------------------------------------------------

(define-module "bard")

(defmethod lookup-module-variable ((nm cl:string)(md module))
  (gethash (intern-for-name nm) (entries md) nil))

(defmethod lookup-module-variable ((nm cl:symbol)(md module))
  (gethash (intern-for-name nm) (entries md) nil))
