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
 GenericFunction
 MethodTable
 SingletonClass
 StandardClass
 TypeSignature
 absent?
 add-slot!
 all-supertypes-of
 allocate-instance
 bard-class-object?
 class-object?
 class-of
 class-direct-slots
 class-direct-supers
 class-precedence-list
 direct-supertypes-of
 instance-of?
 java-class-object?
 make-generic
 no-applicable-method
 present?
 signature
 signature=?
 signature-more-specific?
 singleton
 singleton-value
 subtype-of?
 type-object?
 )

;;; =====================================================================
;;; ABOUT
;;; =====================================================================
;;; bard 0.4 uses an object system based loosely on tiny-clos, with
;;; a built-in metaobject protocol (MOP) that is used to integrate the
;;; underlying Java type system

(require 'list-lib)
(require language)


;;; ---------------------------------------------------------------------
;;; imports
;;; ---------------------------------------------------------------------

(import (rnrs hashtables))

(import (class gnu.lists LList))
(import (class gnu.mapping ProcedureN Symbol))
(import (class java.lang Class))
(import (class java.lang.reflect Type))

;;; =====================================================================
;;; utils
;;; =====================================================================

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

;;; =====================================================================
;;; classes
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; bard slot descriptions
;;; ---------------------------------------------------------------------

(define-simple-class SlotDescription (Object)
  (slot-name init: #!null type: gnu.mapping.Symbol)
  (slot-default-value init: #!null)
  ((*init* nm default)(begin (set! slot-name nm)
                             (set! slot-default-value default))))

;;; ---------------------------------------------------------------------
;;; bard classes
;;; ---------------------------------------------------------------------

(define-simple-class BardClass (Type))

;;; standard-class
;;; ---------------------------------------------------------------------
;;; native bard classes that can be defined by users

(define-simple-class StandardClass (BardClass)
  (direct-superclasses init: '())
  (slot-descriptions init: (make-eqv-hashtable))
  (debug-name::Symbol init: #!null)
  ((*init*) #!void))

;;; singleton-class
;;; ---------------------------------------------------------------------
;;; represents individual constant values as types for dispatch
;;; purposes

(define singletons (make-parameter (make-eqv-hashtable)))
(define %singleton-absent-sentinel (make-parameter (cons '() '())))

(define-simple-class SingletonClass (BardClass)
  (value init: #!void)
  ((*init* val)(set! value val)))

(define (%assert-singleton! val)
  (hashtable-set! (singletons)
                  val (SingletonClass val)))

(define (%find-singleton val)
  (hashtable-ref (singletons) val (%singleton-absent-sentinel)))

;;; =====================================================================
;;; generic functions
;;; =====================================================================

;;; (%apply-gf-to-args gf args default-method)
;;; ---------------------------------------------------------------------
;;; FORWARD REFERENCE

(define %apply-gf-to-args #f)

;;; ---------------------------------------------------------------------
;;; TypeSignature
;;; ---------------------------------------------------------------------

(define-simple-class TypeSignature (Object)
  (types init: '())
  ((*init* typeList)(set! types (map (lambda (x) x) typeList))))

(define (signature . types)
  (TypeSignature types))

(define (signature=? s1::TypeSignature s2::TypeSignature)
  (equal? s1:types s2:types))

(define (signature-more-specific? s1::TypeSignature s2::TypeSignature)
  (if (eqv? s1 s2)
      #t
      (every subtype? s1:types s2:types)))

;;; ---------------------------------------------------------------------
;;; MethodTable
;;; ---------------------------------------------------------------------

(define-simple-class MethodTable (Object)
  (entries init: '()))

;;; ---------------------------------------------------------------------
;;; GenericFunction
;;; ---------------------------------------------------------------------

(define-simple-class GenericFunction (ProcedureN)
  (default-method::ProcedureN init-form: no-applicable-method)
  (methods init-form: #f)
  (debug-name::Symbol init: #!null)
  ((applyN args::Object[]) (%apply-gf-to-args (this) (array->list args) default-method)))

;;; =====================================================================
;;; the MOP API
;;; =====================================================================

;;; (absent? thing)
;;; ---------------------------------------------------------------------
;;; returns true is thing is #!void

(define (absent? x)(eq? x #!void))

;;; (add-method! a-generic a-method)
;;; ---------------------------------------------------------------------
;;; ***

;;; (add-slot! a-class slot-name #!key (default-value #f))
;;; ---------------------------------------------------------------------

(define (add-slot! a-class::StandardClass name
                   #!key (default-value #f))
  (hashtable-set! a-class:slot-descriptions name (SlotDescription name default-value)))


;;; (all-supertypes-of a-type::java.lang.reflect.Type)
;;; ---------------------------------------------------------------------

(define (all-supertypes-of a-type::java.lang.reflect.Type)
  (cond
   ((BardClass? a-type) (error "all-supertypes-of not yet implemented for BardClass"))
   ((java.lang.Class? a-type)
    (let ((supers (direct-supertypes-of a-type)))
      (if (null? supers)
          '()
          (remove-duplicates (append supers (apply append (map all-supertypes-of supers)))))))
   (else '())))


;;; (allocate-instance a-class #!rest initargs)
;;; ---------------------------------------------------------------------

(define (allocate-instance a-class . initargs)
  (apply a-class initargs))

;;; (bard-class-object? thing)
;;; ---------------------------------------------------------------------

(define (bard-class-object? thing)
  (BardClass? thing))

;;; (class-object? thing)
;;; ---------------------------------------------------------------------

(define (class-object? thing)
  (or (BardClass? thing)
      (java.lang.Class? thing)))

;;; (class-of thing)
;;; ---------------------------------------------------------------------

(define (class-of thing)(*:getClass  thing))

;;; (class-precedence-list a-class)
;;; ---------------------------------------------------------------------
;;; ***

(define (class-precedence-list a-class)(all-supertypes-of a-class))

;;; (class-direct-slots a-class)
;;; ---------------------------------------------------------------------

(define (class-direct-slots a-class::Type)
  (cond
   ((StandardClass? a-class)
    (let* ((a-standard-class::StandardClass (as StandardClass a-class))
           (slots a-standard-class:slot-descriptions))
      (vector->list (hashtable-keys slots))))
   ((java.lang.Class? a-class)
    (let* ((a-java-class::Class (as Class a-class))
           (field-array (*:getFields a-java-class)))
      (LList:makeList field-array 0)))
   (else (error "not a class " a-class))))

;;; (class-direct-supers a-class)
;;; ---------------------------------------------------------------------

(define (class-direct-supers a-class)
  (direct-supertypes-of a-class))

;;; (class-slots a-class)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-apply-generic a-generic)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-apply-methods a-generic)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-class-precedence-list a-class)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-getter-and-setter a-class a-slot-name an-allocator)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-method-more-specific? a-generic)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-methods a-generic)
;;; ---------------------------------------------------------------------
;;; ***

;;; (compute-slots a-class)
;;; ---------------------------------------------------------------------
;;; ***

;;; (direct-supertypes-of a-type::java.lang.reflect.Type)
;;; ---------------------------------------------------------------------

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

;;; (generic-methods a-generic)
;;; ---------------------------------------------------------------------
;;; ***

;;; (initialize-instance an-instance &rest initargs &key)
;;; ---------------------------------------------------------------------
;;; ***

;;; (instance-of? thing type-object::java.lang.reflect.Type)
;;; ---------------------------------------------------------------------

(define (instance-of? thing type-object::java.lang.reflect.Type)
  (subtype-of? (class-of thing) type-object))

;;; (java-class-object? thing)
;;; ---------------------------------------------------------------------

(define (java-class-object? thing)
  (java.lang.Class? thing))

;;; (make-class list-of-superclasses list-of-slot-names)
;;; ---------------------------------------------------------------------
;;; ***

;;; (make-generic)
;;; ---------------------------------------------------------------------

(define (make-generic)(GenericFunction))

;;; (make-instance class &rest initargs &key)
;;; ---------------------------------------------------------------------
;;; ***

;;; (make-method list-of-specializers procedure)
;;; ---------------------------------------------------------------------
;;; ***

;;; (method-procedure a-method)
;;; ---------------------------------------------------------------------
;;; ***

;;; (method-specializers a-method)
;;; ---------------------------------------------------------------------
;;; ***

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

;;; (present? thing)
;;; ---------------------------------------------------------------------
;;; returns true is thing is not #!void

(define (present? x)(not (eq? x #!void)))

;;; (singleton thing)
;;; ---------------------------------------------------------------------
;;; returns a singleton class representing thing

(define (singleton val)
  (let ((already (%find-singleton val)))
    (if (eq? already (%singleton-absent-sentinel))
        (begin (%assert-singleton! val)
               (singleton val))
        already)))

;;; (singleton-value s)
;;; ---------------------------------------------------------------------
;;; returns the value represented by the singleton

(define (singleton-value s::SingletonClass) s:value)

;;; (slot-ref an-object a-slot-name)
;;; ---------------------------------------------------------------------
;;; ***

;;; (slot-set! an-object a-slot-name a-new-value)
;;; ---------------------------------------------------------------------
;;; ***

;;; (subtype-of? type1::java.lang.reflect.Type type2::java.lang.reflect.Type)
;;; ---------------------------------------------------------------------
;;; returns a singleton class representing thing

(define (subtype-of? type1::java.lang.reflect.Type type2::java.lang.reflect.Type)
  (cond
   ((eqv? type1 type2) #t)
   ((SingletonClass? type1)(if (SingletonClass? type2)
                               #f
                               (subtype-of? (class-of (singleton-value type1))
                                            type2)))
   ((SingletonClass? type2) #f)
   ((BardClass? type1) (error "subtype-of? not yet implemented for BardClass"))
   ((BardClass? type2) (error "subtype-of? not yet implemented for BardClass"))
   ((and (java.lang.Class? type1)
         (java.lang.Class? type2))
    (let ((class1 (as java.lang.Class type1))
          (class2 (as java.lang.Class type2)))
      (*:isAssignableFrom class2 class1)))
   (else (error (format #f "subtype-of? Unrecognized types in (~s ~s)"
                        type1 type2)))))

;;; (type-object? thing)
;;; ---------------------------------------------------------------------

(define (type-object? thing)(java.lang.reflect.Type? thing))
