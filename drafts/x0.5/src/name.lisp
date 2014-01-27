;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          name.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard names (symbols, keywords, locators)
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)


;;; ---------------------------------------------------------------------
;;; name class
;;; ---------------------------------------------------------------------

(defclass name ()())

(defmethod name? (thing) (declare (ignore thing)) nil)
(defmethod name? ((obj name)) t)

;;; ---------------------------------------------------------------------
;;; keywords
;;; ---------------------------------------------------------------------

(defclass keyword (name)
  ((name :accessor keyword-name :initarg :name)))

(defmethod print-object ((obj keyword)(out stream))
  (princ #\: out)
  (princ (keyword-name obj) out))

(defmethod keyword? (thing) (declare (ignore thing)) nil)
(defmethod keyword? ((obj keyword)) t)

;;; ---------------------------------------------------------------------
;;; symbols
;;; ---------------------------------------------------------------------

(defclass symbol (name)
  ((module :accessor symbol-module :initarg :module)))

(defmethod symbol? (thing) (declare (ignore thing)) nil)
(defmethod symbol? ((obj symbol)) t)

(defmethod print-object ((symbol symbol)(out stream))
  (let* ((symbol-module (if (in-module? symbol (current-module))
                            (current-module)
                            (symbol-module symbol))))
    (if (eql symbol-module (current-module))
        (princ (symbol-name symbol (current-module)) out)
        (progn
          (princ (module-name (symbol-module symbol)) out)
          (princ #\: out)
          (princ (symbol-name symbol) out)))
    symbol))

;;; ---------------------------------------------------------------------
;;; locators
;;; ---------------------------------------------------------------------



