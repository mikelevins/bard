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
 GenericFunction
 signature
 SingletonClass
 StandardClass
 TypeSignature
 all-supertypes-of
 class-of
 direct-supertypes-of
 make-generic
 no-applicable-method
 subtype-of?
 )

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; bard 0.4 uses an object system based loosely on tiny-clos, with
;;; a built-in metaobject protocol (MOP) that is used to integrate the
;;; underlying Java type system

(require 'list-lib)
(require language)

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias ProcedureN gnu.mapping.ProcedureN)
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
;;; utils
;;; ---------------------------------------------------------------------

(define (array->list arr::Object[])
  (let loop ((i 0)
             (result '()))
    (if (< i arr:length)
        (loop (+ i 1)
              (cons (arr i) result))
        (reverse result))))

(define (remove-duplicates ls)
  (let loop ((elts ls)
             (result '()))
    (if (null? elts)
        (reverse result)
        (if (member (car elts) result)
            (loop (cdr elts) result)
            (loop (cdr elts)
                  (cons (car elts)
                        result))))))

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
;;; bard class utilities
;;; ---------------------------------------------------------------------

(define (class-of thing)(*:getClass  thing))

(define (direct-supertypes-of a-type::java.lang.reflect.Type)
  (cond
   ((BardClass? a-type) (error "direct-supertypes-of not yet implemented for BardClass"))
   ((java.lang.Class? a-type)
    (let* ((a-class (as java.lang.Class a-type))
           (super (*:getSuperclass a-class))
           (interfaces (array->list (*:getInterfaces a-class))))
      (if (eqv? #!null super)
          '()
          (if (eqv? super java.lang.Object)
              (append interfaces (list super))
              (cons super interfaces)))))
   (else '())))

(define (all-supertypes-of a-type::java.lang.reflect.Type)
  (cond
   ((BardClass? a-type) (error "all-supertypes-of not yet implemented for BardClass"))
   ((java.lang.Class? a-type)
    (let ((supers (direct-supertypes-of a-type)))
      (if (null? supers)
          '()
          (remove-duplicates (append supers (apply append (map all-supertypes-of supers)))))))
   (else '())))

(define (subtype-of? type1::java.lang.reflect.Type type2::java.lang.reflect.Type)
  (cond
   ((eqv? type1 type2) #t)
   ((BardClass? type1) (error "subtype-of? not yet implemented for BardClass"))
   ((BardClass? type2) (error "subtype-of? not yet implemented for BardClass"))
   ((and (java.lang.Class? type1)
         (java.lang.Class? type2))
    (let ((class1 (as java.lang.Class type1))
          (class2 (as java.lang.Class type2)))
      (*:isAssignableFrom class2 class1)))
   (else (error (format #f "subtype-of? Unrecognized types in (~s ~s)"
                        type1 type2)))))


;;; ---------------------------------------------------------------------
;;; c3
;;; ---------------------------------------------------------------------
;;; the algorithm used to deterministically compute the linearization
;;; of the superclasses of a BardClass. c3 is not used for Java classes,
;;; because Java inheritance works differently.

;;; ---------------------------------------------------------------------
;;; bard generic functions
;;; ---------------------------------------------------------------------

;;; (%apply-gf-to-args gf args default-method)
;;; ---------------------------------------------------------------------
;;; FORWARD REFERENCE

(define %apply-gf-to-args #f)

;;; TypeSignature
;;; ---------------------------------------------------------------------

(define-simple-class TypeSignature (Object)
  (types init: '())
  ((*init* typeList)(set! types (map (lambda (x) x) typeList))))

(define (signature . types)
  (TypeSignature types))

;;; (no-applicable-method . args)
;;; ---------------------------------------------------------------------
;;; called when a generic function is applied to some arguments
;;; and no method applicable to the arguments can be found on the
;;; generic function. Signals an error whose message reports the
;;; arguments and their types.

(define (no-applicable-method . args)
  (error (format #f "No applicable method for arguments ~s with types ~s "
                 args (map (lambda (a)(a:getClass))
                           args))))

;;; CLASS generic-function
;;; ---------------------------------------------------------------------
;;; the class of generic functions. generic functions are procedures
;;; that select a method to run by examining the types of arguments
;;; passed to them.

(define-simple-class GenericFunction (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods init-form: #f)
  ((applyN args::Object[]) (%apply-gf-to-args (this) (array->list args) default-method)))

(define (make-generic)(GenericFunction))


