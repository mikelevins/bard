;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          maps.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       support for the map protocol
;;;;                and data types that support it
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; utilities
;;; ========================================================================

(defun %by-pairs (list &optional (acc nil))
  (if (null list)
      (reverse acc)
      (if (null (cdr list))
          (error "Odd number of arguments to map")
          (%by-pairs (cddr list) 
                    (cons (list (car list)(cadr list)) acc)))))

;;; ========================================================================
;;; PROTOCOL: Map
;;; ========================================================================

(defun empty-map ()(fset:map))

(defun map (&rest k/v-pairs)
  (reduce #'(lambda (u v)
              (fset:with u (car v) (cadr v)))
          (%by-pairs k/v-pairs)
          :initial-value (fset:empty-map)))

(defgeneric associate (m key value))
(defgeneric dissociate (m key))
(defgeneric get (m key &optional default))
(defgeneric keys (m))
(defgeneric merge (&rest maps))
(defgeneric select (m fn))
(defgeneric vals (m))

;;; ========================================================================
;;; IMPLEMENTATION: fset:map
;;; ========================================================================

(defmethod associate ((m fset:map) key value)
  (fset:with m key value))

(defmethod dissociate ((m fset:map) key)
  (fset:less m key))

(defmethod get ((m fset:map) key &optional (default nothing))
  (or (fset:lookup m key)
      default))

(defmethod keys ((m fset:map))
  (fset:stable-sort (fset:domain m)
        'fset:compare))

(defmethod merge (&rest maps)
  (reduce #'(lambda (u v) (fset:map-union u v))
          maps))

(defmethod select ((m fset:map) (fn function))
  (fset:restrict m (filter fn (keys m))))

(defmethod vals ((m fset:map))
  (fset:image (lambda (k) (fset:lookup m k))
              (keys m)))
