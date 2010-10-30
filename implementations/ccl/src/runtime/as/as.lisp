;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          as.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a general-purpose type-conversion utility
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.AS"
  (:nicknames "AS")
  (:export "AS")
  (:use "CL"))

(in-package :as)

;;; =====================================================================
;;; AS conversions
;;; =====================================================================

(defmethod as (class-name value &key &allow-other-keys)
  (if (eql (class-of value) (find-class class-name))
      value
      (error "Don't know how to convert value ~S to class ~S"
             value class-name)))

;;;; ---------------------------------------------------------------------
;;;; NUMBERS
;;;; ---------------------------------------------------------------------

(defmethod as ((class (eql 'cl:number)) (thing cl:character) &key &allow-other-keys)
  (char-code thing))

(defmethod as ((class (eql 'cl:float)) (thing cl:number) &key &allow-other-keys)
  (float thing))

(defmethod as ((class (eql 'cl:integer)) (thing cl:integer) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'cl:integer)) (thing cl:float) &key &allow-other-keys)
  (truncate thing))

(defmethod as ((class (eql 'cl:integer)) (thing cl:ratio) &key &allow-other-keys)
  (truncate thing))

;;;; ---------------------------------------------------------------------
;;;; CHARACTERS
;;;; ---------------------------------------------------------------------

(defmethod as ((class (eql 'cl:character)) (thing cl:number) &key &allow-other-keys)
  (code-char thing))

;;;; ---------------------------------------------------------------------
;;;; SYMBOLS
;;;; ---------------------------------------------------------------------

(defmethod as ((class (eql 'cl:symbol)) (thing cl:string) &key &allow-other-keys)
  (make-symbol thing))

;;;; ---------------------------------------------------------------------
;;;; SEQUENCES
;;;; ---------------------------------------------------------------------

;;; to STRING

(defmethod as ((class (eql 'cl:string)) thing &key &allow-other-keys)
  (format nil "~S" thing))

(defmethod as ((class (eql 'cl:string)) (thing cl:string) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'cl:string)) (thing cl:symbol) &key &allow-other-keys)
  (if (symbol-package thing)
      (concatenate 'string 
                   (package-name (symbol-package thing))
                   "::|"
                   (symbol-name thing)
                   "|")
      (concatenate 'string 
                   "#:|"
                   (symbol-name thing)
                   "|")))

(defmethod as ((class (eql 'cl:string)) (thing cl:character) &key &allow-other-keys)
  (string thing))

(defmethod as ((class (eql 'cl:string)) (thing cl:vector) &key &allow-other-keys)
  (if (every 'characterp thing)
      (coerce thing 'string)
      (format nil "~S" thing)))

(defmethod as ((class (eql 'cl:string)) (thing cl:list) &key &allow-other-keys)
  (if (every 'characterp thing)
      (coerce thing 'string)
      (format nil "~S" thing)))

;;; to VECTOR

(defmethod as ((class (eql 'cl:vector)) (thing cl:vector) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'cl:vector)) (thing cl:list) &key &allow-other-keys)
  (coerce thing 'vector))

(defmethod as ((class (eql 'cl:vector)) (thing cl:string) &key &allow-other-keys)
  (coerce thing 'vector))

;;; to LIST

(defmethod as ((class (eql 'cl:list)) (thing cl:list) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'cl:list)) (thing cl:vector) &key &allow-other-keys)
  (coerce thing 'list))

(defmethod as ((class (eql 'cl:list)) (thing cl:string) &key &allow-other-keys)
  (coerce thing 'list))

;;;; ---------------------------------------------------------------------
;;;; CLASSES
;;;; ---------------------------------------------------------------------

(defmethod as ((class (eql 'cl:class)) (thing cl:symbol) &key &allow-other-keys)
  (find-class thing))


