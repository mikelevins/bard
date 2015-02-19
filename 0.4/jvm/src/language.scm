;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language.scm
;;;; Project:       bard 0.4
;;;; Purpose:       language definition
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export BardLanguage bard-instance)


(require "map.scm")
(require "structure.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Apply gnu.kawa.functions.Apply)
(define-private-alias ApplyToArgs gnu.kawa.functions.ApplyToArgs)
(define-private-alias Environment gnu.mapping.Environment)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias InstanceOf gnu.kawa.reflect.InstanceOf)
(define-private-alias IsEq gnu.kawa.functions.IsEq)
(define-private-alias IsEqv gnu.kawa.functions.IsEqv)
(define-private-alias IsEqual gnu.kawa.functions.IsEqual)
(define-private-alias Map gnu.kawa.functions.Map)
(define-private-alias Not gnu.kawa.functions.Not)
(define-private-alias NumberCompare gnu.kawa.functions.NumberCompare)
(define-private-alias NumberPredicate gnu.kawa.functions.NumberPredicate)
(define-private-alias Procedure gnu.mapping.Procedure)
(define-private-alias Scheme kawa.standard.Scheme)

;;; ---------------------------------------------------------------------
;;; global language environment
;;; ---------------------------------------------------------------------

(define null-environment (make-parameter (Environment:make "null-environment")))

(define %bard-environment (make-parameter #f))
(define (bard-environment)
  (or (%bard-environment)
      (begin
        (%bard-environment (Environment:make "bard-environment" (null-environment)))
        (%bard-environment))))

;;; ---------------------------------------------------------------------
;;; language instance
;;; ---------------------------------------------------------------------

(define %bard-instance (make-parameter #f))
(define (bard-instance)
  (or (%bard-instance)
      (begin
        (%bard-instance (BardLanguage (bard-environment)))
        (%bard-instance))))

;;; ---------------------------------------------------------------------
;;; supporting classes
;;; ---------------------------------------------------------------------

(define-simple-class Box ()
  (value init-form: #!null)
  ((*init* val)(set! value val))
  ((getBox) value)
  ((setBox val) (set! value val)))

;;; ---------------------------------------------------------------------
;;; repl prompt
;;; ---------------------------------------------------------------------

(define (default-prompter port)
  (let ((state (input-port-read-state port)))
    (if (char=? state #\Newline)
	""
	(string-append (if (char=? state #\Space)
			   "bard>"
			   (string-append "  " (make-string 1 state) "---:"))
		       " "))))

;;; ---------------------------------------------------------------------
;;; quitting the Bard process
;;; ---------------------------------------------------------------------

(define (bard-quit)(exit))

;;; ---------------------------------------------------------------------
;;; BardLanguage class
;;; ---------------------------------------------------------------------

(define-simple-class BardLanguage (Scheme)
  (instanceOf :: InstanceOf allocation: 'static access: 'public init-form: #!null)
  (notTrue :: Not allocation: 'static access: 'public init-form: #!null)
  (applyToArgsFn :: ApplyToArgs allocation: 'static access: 'public init-form: #!null)
  (applyFn :: Apply allocation: 'static access: 'public init-form: #!null)
  (identicalFn :: IsEq allocation: 'static access: 'public init-form: #!null)
  (equivalentFn :: IsEqv allocation: 'static access: 'public init-form: #!null)
  (equalFn :: IsEqual allocation: 'static access: 'public init-form: #!null)
  (mapFn :: Map allocation: 'static access: 'public init-form: #!null)
  (foreachFn :: Map allocation: 'static access: 'public init-form: #!null)
  (numEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (isOddFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (isEvenFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (defaultPrompter :: Procedure allocation: 'static access: 'public init-form: default-prompter)
  (bardQuit :: Procedure allocation: 'static access: 'public init-form: bard-quit)
  ;; primitive structures
  ;; ---------------
  (structureBigInteger :: structure allocation: 'static access: 'public
                       init-form: (primitive-structure "big-integer" gnu.math.IntNum
                                                       (lambda (val)
                                                         (if (eq? (val:getClass) gnu.math.IntNum)
                                                             val
                                                             (error (format #f "Not an integer: ~S" val))))))
  (structureBoolean :: structure allocation: 'static access: 'public
                    init-form: (primitive-structure "boolean" java.lang.Boolean
                                                    (lambda (val) (if val #t #f))))
  (structureBox :: structure allocation: 'static access: 'public
                init-form: (primitive-structure "box" Box (lambda (val)(Box val))))
  (structureCons :: structure allocation: 'static access: 'public
                 init-form: (primitive-structure "cons" gnu.lists.Pair 
                                                 (lambda (a b)(gnu.lists.Pair a b))))
  (structureDict :: structure allocation: 'static access: 'public
                init-form: (primitive-structure "dict" HashPMap (lambda (#!rest args)(apply hashpmap args))))
  (structureDoubleFloat :: structure allocation: 'static access: 'public
                        init-form: (primitive-structure "double-float" gnu.math.DFloNum
                                                        (lambda (val)
                                                          (if (eq? (val:getClass) gnu.math.DFloNum)
                                                              val
                                                              (error (format #f "Not a double-float: ~S" val))))))
  (structureKeyword :: structure allocation: 'static access: 'public
                    init-form: (primitive-structure "keyword" gnu.expr.Keyword
                                                    (lambda (val)
                                                      (cond
                                                       ((keyword? val) val)
                                                       ((symbol? val) (string->keyword (symbol->string val)))
                                                       ((string? val) (string->keyword val))
                                                       (#t (error (format #f "Can't convert to a keyword: ~S"
                                                                          val)))))))
  (structureRatio :: structure allocation: 'static access: 'public
                  init-form: (primitive-structure "ratio" gnu.math.IntFraction
                                                  (lambda (val)
                                                    (if (eq? (val:getClass) gnu.math.IntFraction)
                                                        val
                                                        (error (format #f "Not a ratio: ~S" val))))))
  (structureSequence :: structure allocation: 'static access: 'public
                     init-form: (primitive-structure "sequence" org.pcollections.ConsPStack
                                                     (lambda (#!rest args)(org.pcollections.ConsPStack:from args))))
  (structureSingleFloat :: structure allocation: 'static access: 'public
                        init-form: (primitive-structure "single-float" java.lang.Float
                                                        (lambda (val)
                                                          (if (eq? (val:getClass) java.lang.Float)
                                                              val
                                                              (error (format #f "Not a single-float: ~S" val))))))
  (structureSymbol :: structure allocation: 'static access: 'public
                   init-form: (primitive-structure "symbol" gnu.mapping.Symbol
                                                   (lambda (val)
                                                     (cond
                                                      ((keyword? val) (string->symbol (keyword->string val)))
                                                      ((symbol? val) val)
                                                      ((string? val) (string->symbol val))
                                                      (#t (error (format #f "Can't convert to a symbol: ~S"
                                                                         val)))))))
  (structureUnicodeChar :: structure allocation: 'static access: 'public
                        init-form: (primitive-structure "unicode-character" gnu.text.Char
                                                        (lambda (val)
                                                          (cond
                                                           ((char? val) val)
                                                           ((integer? val)(integer->char val))
                                                           (#t (error (format #f "Not a character: ~S" val)))))))
  (structureUnicodeString :: structure allocation: 'static access: 'public
                          init-form: (primitive-structure "unicode-string" java.lang.String
                                                          (lambda (#!rest args)(apply string args))))
  (structureURI :: structure allocation: 'static access: 'public
                init-form: (primitive-structure "uri" URI (lambda (val)(URI val))))
  (structureVector :: structure allocation: 'static access: 'public
                   init-form: (primitive-structure "vector" gnu.lists.FVector
                                                   (lambda (#!rest args)(apply vector args))))

  ;; set up the bard runtime environment
  ;; ----------------------------------

  ((*init* env::Environment)
   (begin (invoke-special kawa.standard.Scheme (this) '*init* env)
          (Environment:setCurrent env)
          ;; named constants
          ;; ---------------
          (*:defAliasStFld (this) "true" "java.lang.Boolean" "TRUE")
          (*:defAliasStFld (this) "false" "java.lang.Boolean" "FALSE")
          (*:defAliasStFld (this) "nothing" "gnu.lists.LList" "Empty")
          ;; special forms
          ;; -------------
          (*:defSntxStFld (this) "^" "kawa.standard.SchemeCompilation" "lambda")
          (*:defSntxStFld (this) "begin" "kawa.standard.begin" "begin")
          (*:defSntxStFld (this) "%define" "kawa.standard.define" "defineRaw")
          (*:defSntxStFld (this) "def" "kawa.lib.prim_syntax" "define")
          (*:defSntxStFld (this) "if" "kawa.lib.prim_syntax")
          (*:defSntxStFld (this) "quote" "kawa.lang.Quote" "plainQuote")
          ;; ;; built-in structures
          ;; ;; --------
          (*:defProcStFld (this) "big-integer" "BardLanguage" "structureBigInteger")
          (*:defProcStFld (this) "boolean" "BardLanguage" "structureBoolean")
          (*:defProcStFld (this) "box" "BardLanguage" "structureBox")
          (*:defProcStFld (this) "cons" "BardLanguage" "structureCons")
          (*:defProcStFld (this) "dict" "BardLanguage" "structureDict")
          (*:defProcStFld (this) "double-float" "BardLanguage" "structureDoubleFloat")
          (*:defProcStFld (this) "keyword" "BardLanguage" "structureKeyword")
          (*:defProcStFld (this) "ratio" "BardLanguage" "structureRatio")
          (*:defProcStFld (this) "sequence" "BardLanguage" "structureSequence")
          (*:defProcStFld (this) "single-float" "BardLanguage" "structureSingleFloat")
          (*:defProcStFld (this) "symbol" "BardLanguage" "structureSymbol")
          (*:defProcStFld (this) "unicode-character" "BardLanguage" "structureUnicodeChar")
          (*:defProcStFld (this) "unicode-string" "BardLanguage" "structureUnicodeString")
          (*:defProcStFld (this) "uri" "BardLanguage" "structureURI")
          (*:defProcStFld (this) "vector" "BardLanguage" "structureVector")
          ;;;; built-in procedures
          ;;;; --------
          ;;;; repl prompt
          (*:defProcStFld (this) "default-prompter" "BardLanguage" "defaultPrompter")
          ;;;; quit
          (*:defProcStFld (this) "quit" "BardLanguage" "bardQuit")
          ;;;; instance?
          ;;;; TODO: make this work properly with structures; currently it works only
          ;;;;       with Java classes
          (begin (set! instanceOf (InstanceOf (this) "instance?"))
                 (*:defProcStFld (this) "instance?" "BardLanguage" "instanceOf"))
          ;; not
          (begin (set! notTrue (Not (this) "not"))
                 (*:defProcStFld (this) "not" "BardLanguage" "notTrue"))
          ;; apply
          (begin (set! applyToArgsFn (ApplyToArgs "apply-to-args" (this)))
                 (set! applyFn (gnu.kawa.functions.Apply "apply" applyToArgsFn))
                 (*:defProcStFld (this) "apply" "BardLanguage" "applyFn"))
          ;; identical?
          (begin (set! identicalFn (IsEq (this) "identical?"))
                 (*:defProcStFld (this) "identical?" "BardLanguage" "identicalFn"))
          ;; equivalent?
          (begin (set! equivalentFn (IsEqv (this) "equivalent?" identicalFn))
                 (*:defProcStFld (this) "equivalent?" "BardLanguage" "equivalentFn"))
          ;; equal?
          (begin (set! equalFn (IsEqual (this) "equal?"))
                 (*:defProcStFld (this) "equal?" "BardLanguage" "equalFn"))
          ;; for-each
          (begin (set! mapFn (Map #f applyToArgsFn identicalFn))
                 (*:defProcStFld (this) "for-each" "BardLanguage" "foreachFn"))
          ;; =
          (begin (set! numEqFn (NumberCompare:make (this) "=" NumberCompare:TRUE_IF_EQU))
                 (*:defProcStFld (this) "=" "BardLanguage" "numEqFn"))
          ;; <
          (begin (set! numLessFn (NumberCompare:make (this) "<" NumberCompare:TRUE_IF_LSS))
                 (*:defProcStFld (this) "<" "BardLanguage" "numLessFn"))
          ;; <=
          (begin (set! numLessEqFn (NumberCompare:make
                                    (this) "<="
                                    (bitwise-ior NumberCompare:TRUE_IF_LSS
                                                 NumberCompare:TRUE_IF_EQU)))
                 (*:defProcStFld (this) "<=" "BardLanguage" "numLessEqFn"))
          ;; >
          (begin (set! numGreaterFn (NumberCompare:make (this) ">" NumberCompare:TRUE_IF_GRT))
                 (*:defProcStFld (this) ">" "BardLanguage" "numGreaterFn"))
          ;; >=
          (begin (set! numGreaterEqFn (NumberCompare:make
                                       (this) ">="
                                       (bitwise-ior NumberCompare:TRUE_IF_GRT
                                                    NumberCompare:TRUE_IF_EQU)))
                 (*:defProcStFld (this) ">=" "BardLanguage" "numGreaterEqFn"))
          ;; odd?
          (begin (set! isOddFn (NumberPredicate (this) "odd?" NumberPredicate:ODD))
                 (*:defProcStFld (this) "odd?" "BardLanguage" "isOddFn"))
          (begin (set! isEvenFn (NumberPredicate (this) "even?" NumberPredicate:EVEN))
                 (*:defProcStFld (this) "even?" "BardLanguage" "isEvenFn"))
          ;; even?
          ))

  ;; ----------------------------------

  ((getInstance) allocation: 'static (bard-instance))
  ((builtin) allocation: 'static (bard-environment)))
