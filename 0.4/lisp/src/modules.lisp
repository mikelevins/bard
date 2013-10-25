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

(defun make-mvar (name id &key (mutable t) (exported nil) (import-from nil) (import-name nil))
  (make-instance '<mvar> name id 
                 :mutable mutable 
                 :exported exported
                 :import-from import-from
                 :import-name import-name))

;;; ---------------------------------------------------------------------
;;; module names
;;; ---------------------------------------------------------------------

(defun make-module (gs)(make-instance '<module> :globals gs))

(defun ensure-variable-with-id (globals id)
  (declare (ignore globals id))
  (not-yet-implemented 'ensure-variable-with-id))

(defmethod add-module-variable! ((module <module>)(id integer)(name symbol) 
                          &key (mutable t) (exported nil) (import-from nil) (import-name nil))
  (let ((id (ensure-variable-with-id (globals module) id))
        (mvar (make-mvar name id :mutable mutable :exported exported :import-from import-from :import-name import-name)))
    (setf (gethash name (variables-by-name module)) mvar)
    (setf (gethash id (variables-by-id module)) mvar)
    module))

(defmethod remove-module-variable! ((module <module>)(name symbol))
  (let* ((mvar (gethash name (variables-by-name module)))
         (id (id mvar)))
    (remhash name (variables-by-name module))
    (remhash id (variables-by-id module))
    module))

(defmethod lookup-module-variable ((module <module>)(name symbol))
  (let* ((mvar (gethash name (variables-by-name module)))
         (id (id mvar)))
    (get-global (globals module) id)))

(defmethod find-module ((comp <compiler>)(mname symbol))
  (gethash mname (modules comp)))

(defmethod assert-module! ((comp <compiler>)(mname symbol))
  (let ((already (gethash mname (modules comp))))
    (or already
        (let ((m (make-module (globals comp))))
          (setf (gethash mname (modules comp)) m)
          m))))

;;; ---------------------------------------------------------------------
;;; initializing standard modules
;;; ---------------------------------------------------------------------

(defun make-standard-modules (comp)
  (assert-module! comp 'bard-modules::bard.lang)
  (assert-module! comp 'bard-modules::bard.user)
  t)



