;;;; ***********************************************************************
;;;; Name:          structure.scm
;;;; Project:       bard 0.4
;;;; Purpose:       bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export structure primitive-structure)

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

(define (structure? x)
  (instance? x structure))

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

;;; end
;;; undefined

;;; Character
;;; ---------------------------------------------------------------------

;;; ascii-character

;;; Number
;;; ---------------------------------------------------------------------

;;; complex

;;; Name
;;; ---------------------------------------------------------------------


;;; Pair
;;; ---------------------------------------------------------------------

;;; List
;;; ---------------------------------------------------------------------

;;; adjustable-vector
;;; word-vector

;;; arrays
;;; ---------------------------------------------------------------------

;;; array
;;; word-array

;;; Map
;;; ---------------------------------------------------------------------

;;; tree-map

(define tree-map
  (primitive-structure "tree-map" HashPMap
                       (lambda (#!rest args)(apply hashpmap args))))


;;; hash-table

(define hash-table
  (primitive-structure "hash-table" java.util.concurrent.ConcurrentHashMap
                       (lambda (#!rest args)
                         (let ((hmap (java.util.concurrent.ConcurrentHashMap)))
                           (let loop ((plist args))
                             (if (null? plist)
                                 hmap
                                 (if (null? (cdr plist))
                                     (error (format #f "Malformed initargs to hash-table: ~s"
                                                    args))
                                     (begin (*:put hmap (car plist)(cadr plist))
                                            (loop (cddr plist))))))))))


;;; protocol


(define-simple-class BardProtocol ()
  (name init-form: #!null)
  ((getName) name)
  ((setName nm) (set! name nm))

  (variables init-form: (java.util.concurrent.ConcurrentHashMap))
  ((getVariables) variables))

(define protocol
  (primitive-structure "protocol" BardProtocol
                       (lambda (name #!rest args)
                         (let* ((proto (BardProtocol))
                                (hmap (*:getVariables proto)))
                           (*:setName proto name)
                           (let loop ((plist args))
                             (if (null? plist)
                                 proto
                                 (if (null? (cdr plist))
                                     (error (format #f "Malformed initargs to protocol: ~s"
                                                    args))
                                     (begin (*:put hmap (car plist)(cadr plist))
                                            (loop (cddr plist))))))))))

;;; Stream
;;; ---------------------------------------------------------------------

;;; input-stream
;;; output-stream
;;; generator
;;; io-stream

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
  (entries access: 'public type: java.util.AbstractList init-form: #!null)
  ((getEntries) entries)
  ((getTupleRef index)(*:get entries index)))

(define-simple-class MutableTupleInstance (TupleInstance)
  ((setTupleRef index val)(*:set entries index val)))

(define-simple-class TupleType ()
  (name access: 'public init-form: #!null)
  ((getName) name)

  (element-type access: 'public init-form: #!null)
  ((getElementType) element-type)

  (min-count access: 'public init-form: -1)
  ((getMinCount) min-count)

  (max-count access: 'public init-form: -1)
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



;;; procedures
;;; ---------------------------------------------------------------------

;;; method
;;; special-case method so we can use it both as an alias for ^ (lambda)
;;; and also as the structure (type) of simple procedures

(define-syntax method
  (syntax-rules ()
    ((method params expr ...)
     (lambda params expr ...))))

;;; we'll write dispatch code for type discriminators that recognizes
;;; bard-method as a type

(define bard-method
  method)

;;; function

(define-simple-class BardFunction ()
  (name init-form: #!null)
  ((getName) name)
  ((setName nm) (set! name nm))

  (methods init-form: #!null)
  ((getMethods) methods)
  ((setMethods ms) (set! methods ms))

  (input-types init-form: '())
  ((getInputTypes) input-types)
  ((setInputTypes tps) (set! input-types tps))

  (output-types init-form: '())
  ((getOutputTypes) output-types)
  ((setOutputTypes tps) (set! output-types tps)))

(define function
  (primitive-structure "function" BardFunction
                       (lambda (nm inputs outputs)
                         (let ((fn (BardFunction)))
                           (*:setName fn nm)
                           (*:setInputTypes fn inputs)
                           (*:setOutputTypes fn outputs)
                           fn))))


;;; macro
;;; special-form

(define special-form
  (type-constructor "special-form" (lambda (#!rest args)(error "Objects of type special-form cannot be created at runtime."))))

