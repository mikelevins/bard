;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          mop.scm
;;;; Project:       bard 0.4
;;;; Purpose:       bard's object system
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 BardClass
 BuiltInClass
 SingletonClass
 StandardClass
 )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; bard 0.4 uses an object system based loosely on tiny-clos, with
;;; a built-in metaobject protocol (MOP) that is used to integrate the
;;; underlying Java type system

(require "language.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Type java.lang.reflect.Type)

;;; ---------------------------------------------------------------------
;;; API
;;; ---------------------------------------------------------------------
;;; 
;;; (add-method! a-generic a-method)
;;; (allocate-instance a-class . initargs)
;;; (class-precedence-list a-class)
;;; (class-direct-slots a-class)
;;; (class-direct-supers a-class)
;;; (class-of thing)
;;; (class-slots a-class)
;;; (compute-apply-generic a-generic)
;;; (compute-apply-methods a-generic)
;;; (compute-class-precedence-list a-class)
;;; (compute-getter-and-setter a-class a-slot-name an-allocator)
;;; (compute-method-more-specific? a-generic)
;;; (compute-methods a-generic)
;;; (compute-slots a-class)
;;; (generic-methods a-generic)
;;; (initialize-instance an-instance &rest initargs &key)
;;; (make-instance class &rest initargs &key)
;;; (make-class list-of-superclasses list-of-slot-names)
;;; (make-generic)
;;; (make-method list-of-specializers procedure)
;;; (method-procedure a-method)
;;; (method-specializers a-method)
;;; (slot-ref an-object a-slot-name)
;;; (slot-set! an-object a-slot-name a-new-value)

;;; ---------------------------------------------------------------------
;;; bard classes
;;; ---------------------------------------------------------------------

(define-simple-class BardClass (Type))

;;; built-in-class
;;; ---------------------------------------------------------------------
;;; aliases for Kawa and Java types

(define-simple-class BuiltInClass (BardClass))

;;; standard-class
;;; ---------------------------------------------------------------------
;;; native bard classes that can be defined by users

(define-simple-class StandardClass (BardClass))

;;; singleton-class
;;; ---------------------------------------------------------------------
;;; represents individual constant values as types for dispatch
;;; purposes

(define-simple-class SingletonClass (BardClass))

;;; ---------------------------------------------------------------------
;;; bard generic functions
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; c3
;;; ---------------------------------------------------------------------
;;; the algorithm used to deterministically compute the linearization
;;; of a class' superclasses


