;;;; ***********************************************************************
;;;;
;;;; Name:          structure-registry.scm
;;;; Project:       Bard
;;;; Purpose:       registry of defined structures 
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************


(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; the structure registry
;;; ---------------------------------------------------------------------
;;; the structure registry is a table that maps type numbers to structure
;;; objects. A structure object is a data structure that represents
;;; the concrete type of a set of values.
;;;
;;; NOTE: the proper implementation of structure objects will have
;;;       internal structure. bard structures are funcallable;
;;;       a structure is also a constructor function for its instances
;;;       however, for now, during bootstrapping, I'm just going to
;;;       store the names of the structures in the table

(define +the-structure-registry+ (make-table test: eqv?))

(define (register-structure! structure-object type-number)
  (table-set! +the-structure-registry+ type-number structure-object))

(define (structure-of thing)
  (table-ref +the-structure-registry+ 
             (bard-structure-number-of thing)))
