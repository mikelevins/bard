;;;; ***********************************************************************
;;;; Name:          structure.scm
;;;; Project:       bard 0.4
;;;; Purpose:       representations of structure types
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export BardClass structure structure? primitive-structure)

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

;;; Dictionary
;;; ---------------------------------------------------------------------

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
  ((*init* nm)(begin (set! name nm))))

(define bard-class 
  (type-constructor "class" (lambda (nm)(BardClass nm))))

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
  (type-constructor "special-form"
                    (lambda (#!rest args)
                      (error "Objects of type special-form cannot be created at runtime."))))

