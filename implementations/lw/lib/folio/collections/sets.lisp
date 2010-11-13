;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sets.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional sets
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.SETS"
  (:use :as :cl)
  (:nicknames "SET")
  (:shadow "ADJOIN" "INTERSECTION" "UNION")
  (:export "ADJOIN" "CONTAINS?" "DIFFERENCE" "INTERSECTION" "MAKE" "SUBSET?" "UNION"))

(in-package :set)

;;; =====================================================================
;;; AS methods
;;; =====================================================================

(defmethod as ((class (eql 'fset:set)) (thing fset:set) &key &allow-other-keys)
  thing)

(defmethod as ((class (eql 'fset:set)) (thing fset:seq) &key &allow-other-keys)
  (fset:convert 'fset:set thing))

(defmethod as ((class (eql 'fset:seq)) (thing fset:set) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'fset:set)) (thing cl:list) &key &allow-other-keys)
  (fset:convert 'fset:set thing))

(defmethod as ((class (eql 'cl:list)) (thing fset:set) &key &allow-other-keys)
  (fset:convert 'cl:list thing))

(defmethod as ((class (eql 'fset:set)) (thing cl:vector) &key &allow-other-keys)
  (fset:convert 'fset:set thing))

(defmethod as ((class (eql 'cl:vector)) (thing fset:set) &key &allow-other-keys)
  (fset:convert 'cl:vector thing))

(defmethod as ((class (eql 'fset:set)) (thing cl:string) &key &allow-other-keys)
  (fset:convert 'fset:set thing))

(defmethod as ((class (eql 'cl:string)) (thing fset:set) &key &allow-other-keys)
  (coerce (fset:convert 'cl:vector thing) 'string))

;;; =====================================================================
;;; SET API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; adjoin
;;; ---------------------------------------------------------------------

(defmethod adjoin (x (set cl:list) &key (test 'eql) &allow-other-keys)
  (cl:adjoin x set :test test))

(defmethod adjoin (x (set fset:set) &key &allow-other-keys)
  (fset:with set x))

;;; ---------------------------------------------------------------------
;;; contains?
;;; ---------------------------------------------------------------------

(defmethod contains? ((set cl:list) x &key (test 'eql) &allow-other-keys)
  (member x set :test test))

(defmethod contains? ((set fset:set) x &key &allow-other-keys)
  (fset:contains? set x))

;;; ---------------------------------------------------------------------
;;; difference
;;; ---------------------------------------------------------------------

(defmethod difference ((set1 cl:list)(set2 cl:list) &key (test 'eql) &allow-other-keys)
  (cl:set-difference set1 set2 :test test))

(defmethod difference ((set1 fset:set)(set2 fset:set) &key &allow-other-keys)
  (fset:set-difference set1 set2))

(defmethod difference ((set1 fset:set)(set2 cl:list) &key  &allow-other-keys)
  (fset:set-difference set1 (as 'fset:set set2)))

(defmethod difference ((set1 cl:list)(set2 fset:set) &key (test 'eql) &allow-other-keys)
  (cl:set-difference set1 (as 'list set2) :test test))

;;; ---------------------------------------------------------------------
;;; intersection
;;; ---------------------------------------------------------------------

(defmethod intersection ((set1 cl:list)(set2 cl:list) &key (test 'eql) &allow-other-keys)
  (cl:intersection set1 set2 :test test))

(defmethod intersection ((set1 fset:set)(set2 fset:set) &key &allow-other-keys)
  (fset:intersection set1 set2))

(defmethod intersection ((set1 fset:set)(set2 cl:list) &key &allow-other-keys)
  (intersection set1 (as 'fset:set set2)))

(defmethod intersection ((set1 cl:list)(set2 fset:set) &key (test 'eql) &allow-other-keys)
  (intersection set1 (as 'list set2) :test test))

;;; ---------------------------------------------------------------------
;;; make
;;; ---------------------------------------------------------------------

(defun make (&rest args)(fset:convert 'fset:set args))

;;; ---------------------------------------------------------------------
;;; subset?
;;; ---------------------------------------------------------------------

(defmethod subset? ((set1 cl:list)(set2 cl:list) &key (test 'eql) &allow-other-keys)
  (cl:subsetp set1 set2 :test test))

(defmethod subset? ((set1 fset:set)(set2 fset:set) &key &allow-other-keys)
  (fset:subset? set1 set2))

(defmethod subset? ((set1 fset:set)(set2 cl:list) &key &allow-other-keys)
  (fset:subset? set1 (as 'fset:set set2)))

(defmethod subset? ((set1 cl:list)(set2 fset:set) &key (test 'eql) &allow-other-keys)
  (cl:subsetp set1 (as 'list set2) :test test))

;;; ---------------------------------------------------------------------
;;; union
;;; ---------------------------------------------------------------------

(defmethod union ((set1 cl:list)(set2 cl:list) &key (test 'eql) &allow-other-keys)
  (cl:union set1 set2 :test test))

(defmethod union ((set1 fset:set)(set2 fset:set) &key &allow-other-keys)
  (fset:union set1 set2))

(defmethod union ((set1 fset:set)(set2 cl:list) &key &allow-other-keys)
  (union set1 (as 'fset:set set2)))

(defmethod union ((set1 cl:list)(set2 fset:set) &key (test 'eql) &allow-other-keys)
  (cl:union set1 (as 'list set2) :test test))
