;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-string.lisp
;;;; Project:       Bard
;;;; Purpose:       string primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; string constructors
;;; ---------------------------------------------------------------------

(defmethod string.append ((sl string)(sr string))
  (concatenate 'string sl sr))

;;; ---------------------------------------------------------------------
;;; string accessors
;;; ---------------------------------------------------------------------

(defmethod string.slice ((s string) (start integer) (end integer))
  (subseq s start end))

(defmethod string.first ((s string))
  (elt s 0))

(defmethod string.rest ((s string))
  (subseq s 1))

(defmethod string.last ((s string))
  (elt s (- (length s) 1)))

;;; ---------------------------------------------------------------------
;;; string converters
;;; ---------------------------------------------------------------------

(defmethod as-string ((x null))
  (declare (ignore x))
  "")

(defmethod as-string ((ch character))
  (string ch))

(defmethod as-string ((ls cons))
  (if (every 'characterp ls)
      (coerce ls 'string)
      (format nil "~a" (value->literal-string ls))))

(defmethod as-string ((s symbol))
  (symbol-name s))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|as-string| 1
    (make-prim :name 'bard-symbols::|as-string|
               :n-args 1
               :opcode 'bard::as-string
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|string.append| 2
    (make-prim :name 'bard-symbols::|string.append|
               :n-args 2
               :opcode 'bard::string.append
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|string.slice| 3
    (make-prim :name 'bard-symbols::|string.slice|
               :n-args 3
               :opcode 'bard::string.slice
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|string.first| 1
    (make-prim :name 'bard-symbols::|string.first|
               :n-args 1
               :opcode 'bard::string.first
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|string.rest| 1
    (make-prim :name 'bard-symbols::|string.rest|
               :n-args 1
               :opcode 'bard::string.rest
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|string.last| 1
    (make-prim :name 'bard-symbols::|string.last|
               :n-args 1
               :opcode 'bard::string.last
               :always t
               :side-effects nil))

