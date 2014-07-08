;;;; ***********************************************************************
;;;; Name:          structure.scm
;;;; Project:       bard 0.4
;;;; Purpose:       bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export structure primitive-structure)

(define-private-alias LList gnu.lists.LList)
(define-private-alias PersistentHashMap com.github.krukow.clj_lang.PersistentHashMap)
(define-private-alias ProcedureN gnu.mapping.ProcedureN)
(define-private-alias Type java.lang.reflect.Type)

;;; ---------------------------------------------------------------------
;;; structure
;;; ---------------------------------------------------------------------
;;; abstract class of Bard structures

(define-simple-class structure (ProcedureN Type)
  (name init-form: #!null)
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



