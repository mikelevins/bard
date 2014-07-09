;;;; ***********************************************************************
;;;; Name:          structure.scm
;;;; Project:       bard 0.4
;;;; Purpose:       bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export structure primitive-structure singleton)

(require "map.scm")

(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias LList gnu.lists.LList)
(define-private-alias Map java.util.Map)
(define-private-alias ProcedureN gnu.mapping.ProcedureN)
(define-private-alias Type java.lang.reflect.Type)

;;; ---------------------------------------------------------------------
;;; structure
;;; ---------------------------------------------------------------------
;;; abstract class of Bard structures

(define-simple-class structure (ProcedureN Type)
  (name init-form: #!null)
  ((getName) name)
  ((applyN args::Object[]) #!abstract))

(define (%construct constructor-fn initargs)
  (apply constructor-fn initargs))

(define (bard-structure? x)
  (instance? x bard-structure))

;;; ---------------------------------------------------------------------
;;; primitive-structure
;;; ---------------------------------------------------------------------
;;; structures implemented by the host runtime

(define-simple-class primitive-structure (structure)
  (native-type init-form: #!null)
  (native-constructor init-form: #!null)
  ((*init* nm ntype constr)(begin (set! name nm)
                                  (set! native-type ntype)
                                  (set! native-constructor constr)))
  ((applyN args::Object[]) (let ((initargs (LList:makeList args 0)))
                             (%construct native-constructor initargs))))

;;; simple structures
;;; ---------------------------------------------------------------------

(define none
  (primitive-structure "none" gnu.lists.EmptyList
                       (lambda () '())))

(define boolean
  (primitive-structure "boolean" java.lang.Boolean
                       (lambda (val) (if val #t #f))))

(define unicode-character
  (primitive-structure "unicode-character" gnu.text.Char
                       (lambda (val)
                         (cond
                          ((char? val) val)
                          ((integer? val)(integer->char val))
                          (#t (error (format #f "Not a character: ~S" val)))))))

;;; numbers
;;; ---------------------------------------------------------------------

;;; example: 5
(define big-integer
  (primitive-structure "big-integer" gnu.math.IntNum
                       (lambda (val)
                         (if (eq? (val:getClass) gnu.math.IntNum)
                             val
                             (error (format #f "Not an integer: ~S" val))))))


;;; example: 2.0f0
(define single-float
  (primitive-structure "single-float" java.lang.Float
                       (lambda (val)
                         (if (eq? (val:getClass) java.lang.Float)
                             val
                             (error (format #f "Not a single-float: ~S" val))))))

;;; example: 2.0
(define double-float
  (primitive-structure "double-float" gnu.math.DFloNum
                       (lambda (val)
                         (if (eq? (val:getClass) gnu.math.DFloNum)
                             val
                             (error (format #f "Not a double-float: ~S" val))))))

;;; example: 2/3
(define ratio
  (primitive-structure "ratio" gnu.math.IntFraction
                       (lambda (val)
                         (if (eq? (val:getClass) gnu.math.IntFraction)
                             val
                             (error (format #f "Not a ratio: ~S" val))))))

;;; names
;;; ---------------------------------------------------------------------

(define bard-symbol
  (primitive-structure "symbol" gnu.mapping.Symbol
                       (lambda (val)
                         (cond
                          ((keyword? val) (string->symbol (keyword->string val)))
                          ((symbol? val) val)
                          ((string? val) (string->symbol val))
                          (#t (error (format #f "Can't convert to a symbol: ~S"
                                             val)))))))

(define bard-keyword
  (primitive-structure "keyword" gnu.expr.Keyword
                       (lambda (val)
                         (cond
                          ((keyword? val) val)
                          ((symbol? val) (string->keyword (symbol->string val)))
                          ((string? val) (string->keyword val))
                          (#t (error (format #f "Can't convert to a keyword: ~S"
                                             val)))))))

(define uri
  (primitive-structure "uri" URI (lambda (val)(URI val))))

;;; lists
;;; ---------------------------------------------------------------------

(define-simple-class Box ()
  (value init-form: #!null)
  ((*init* val)(set! value val))
  ((getBox) value)
  ((setBox val) (set! value val)))

(define box
  (primitive-structure "box" Box 
                       (lambda (val)(Box val))))


(define bard-cons
  (primitive-structure "cons" gnu.lists.Pair 
                       (lambda (a b)(gnu.lists.Pair a b))))

(define bard-vector
  (primitive-structure "vector" gnu.lists.FVector
                       (lambda (#!rest args)(apply vector args))))

(define expanding-vector
  (primitive-structure "vector" gnu.lists.FVector
                       (lambda (#!rest args)(apply vector args))))

(define sequence
  (primitive-structure "sequence" org.pcollections.ConsPStack
                       (lambda (#!rest args)(org.pcollections.ConsPStack:from args))))

(define unicode-string
  (primitive-structure "unicode-string" java.lang.String
                       (lambda (#!rest args)(apply string args))))


;;; arrays
;;; ---------------------------------------------------------------------

;;; array
;;; word-array

;;; maps
;;; ---------------------------------------------------------------------

;;; tree-map
;;; hash-table
;;; protocol

;;; streams
;;; ---------------------------------------------------------------------

;;; input-stream
;;; output-stream
;;; generator
;;; io-stream

;;; procedures
;;; ---------------------------------------------------------------------

;;; method
;;; function
;;; macro
;;; special-form

;;; processes
;;; ---------------------------------------------------------------------

;;; thread
;;; process
;;; bard

;;; conditions
;;; ---------------------------------------------------------------------

;;; warning
;;; error
;;; restart
;;; abort
;;; exit

;;; ---------------------------------------------------------------------
;;; type constructors
;;; ---------------------------------------------------------------------

(define-simple-class type-constructor (structure)
  (type-constructor init-form: #!null)
  ((*init* nm constr)(begin (set! name nm)
                            (set! type-constructor constr)))
  ((applyN args::Object[]) (let ((initargs (LList:makeList args 0)))
                             (%construct type-constructor initargs))))

;;; class
;;; ---------------------------------------------------------------------

(define-simple-class BardClass ()
  (name init-form: #!null)
  ((getName) name)

  (direct-superclasses init-form: '())
  ((getDirectSuperclasses) direct-superclasses)
  ((setDirectSuperclasses classes) (set! direct-superclasses classes))
  
  ((*init* nm cls)(begin (set! name nm)
                         (set! direct-superclasses cls))))

(define bard-class 
  (type-constructor "class" (lambda (nm cls)(BardClass nm cls))))

;;; singleton
;;; ---------------------------------------------------------------------

(define-simple-class SingletonType ()
  (singletons type: HashPMap allocation: 'static init-form: (hashpmap))
  ((getSingletons) allocation: 'static singletons)
  ((setSingletons sings) allocation: 'static (set! singletons sings))

  (value init-form: #!null)
  ((getValue) value)

  ((*init* val)(set! value val)))

(define singleton
  (type-constructor "singleton"
                    (lambda (val)
                      (let ((singletons::HashPMap (SingletonType:getSingletons)))
                        (if (*:containsKey singletons val)
                            (*:get singletons val)
                            (let ((sing (SingletonType val)))
                              (SingletonType:setSingletons (*:plus singletons val sing))
                              sing))))))

;;; record
;;; ---------------------------------------------------------------------

;;; tuple
;;; ---------------------------------------------------------------------
;;; TODO: make TupleInsstance into a Procedure so that tuple instances
;;; can beused as constructors

(define-simple-class TupleInstance ()
  (type init-form: #!null)
  ((getType) type)
  (entries type: java.util.AbstractList init-form: #!null)
  ((getEntries) entries)
  ((getTupleRef index)(*:get entries index)))

(define-simple-class MutableTupleInstance (TupleInstance)
  ((setTupleRef index val)(*:set entries index val)))

(define-simple-class TupleType ()
  (name init-form: #!null)
  ((getName) name)

  (element-type init-form: #!null)
  ((getElementType) element-type)

  (min-count init-form: -1)
  ((getMinCount) min-count)

  (max-count init-form: -1)
  ((getMaxCount) max-count)

  ((*init*) #!void)
  ((*init* nm tp min max)(begin (set! name nm)
                                (set! element-type tp)
                                (set! min-count min)
                                (set! max-count max))))

(define-simple-class MutableTupleType (TupleType)
  ((*init* nm tp min max)(begin (set! name nm)
                                (set! element-type tp)
                                (set! min-count min)
                                (set! max-count max))))

;;; (tuple "foo" java.lang.Object 0 4 #f)
;;; (tuple "bar" java.lang.Object 0 4 #t)
(define tuple
  (type-constructor "tuple" (lambda (name item-type min-count max-count mutable?)
                              (if mutable?
                                  (MutableTupleType name item-type min-count max-count)
                                  (TupleType name item-type min-count max-count)))))

;;; synonym
;;; ---------------------------------------------------------------------

(define-simple-class TypeSynonym ()
  (original-type init-form: #!null)
  (name init-form: #!null)
  ((getName) name)

  ((*init* nm tp)(begin (set! name nm)
                        (set! original-type tp)))
  ((getOriginalType) original-type)
  ((setOriginalType tp) (set! original-type tp)))

;;; (define dollars (synonym 'dollars gnu.math.DFloNum))
(define synonym
  (type-constructor "synonym" (lambda (nm tp)(TypeSynonym nm tp))))


