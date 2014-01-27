;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard map api
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defun make-map (plist)
  (fset:convert 'fset:map (loop for tail on plist by 'cddr collect (cons (car tail)(cadr tail)))))

(defun map (&rest plist)
  (make-map plist))

(defmethod get-key ((map fset:map) key &key (default nil))
  (let* ((absent (fset:map-default map))
         (val (fset:@ map key)))
    (if (equal val absent)
        default
        val)))

(defmethod keys ((map fset:map))
  (fset:domain map))

(defmethod merge ((map1 fset:map)(map2 fset:map))
  (fset:map-union map1 map2))

(defmethod put-key ((map fset:map) key val)
  (fset:with map key val))

(defmethod remove-key ((map fset:map) key)
  (fset:less map key))

(defmethod values ((map fset:map))
  (let ((keys (fset:convert 'list (keys map))))
    (mapcar (lambda (key)(get-key map key))
            keys)))

