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

(module-export Bard bard-instance)


(require "map.scm")
(require "structure.scm")

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Apply gnu.kawa.functions.Apply)
(define-private-alias ApplyToArgs gnu.kawa.functions.ApplyToArgs)
(define-private-alias ClassType gnu.bytecode.ClassType)
(define-private-alias Environment gnu.mapping.Environment)
(define-private-alias HashPMap org.pcollections.HashPMap)
(define-private-alias InstanceOf gnu.kawa.reflect.InstanceOf)
(define-private-alias IsEq gnu.kawa.functions.IsEq)
(define-private-alias IsEqv gnu.kawa.functions.IsEqv)
(define-private-alias IsEqual gnu.kawa.functions.IsEqual)
(define-private-alias LispLanguage gnu.kawa.lispexpr.LispLanguage)
(define-private-alias Map gnu.kawa.functions.Map)
(define-private-alias Not gnu.kawa.functions.Not)
(define-private-alias NumberCompare gnu.kawa.functions.NumberCompare)
(define-private-alias NumberPredicate gnu.kawa.functions.NumberPredicate)
(define-private-alias Procedure gnu.mapping.Procedure)
(define-private-alias ReaderDispatch gnu.kawa.lispexpr.ReaderDispatch)
(define-private-alias ReaderDispatchSyntaxQuote gnu.kawa.lispexpr.ReaderDispatchSyntaxQuote)
(define-private-alias ReaderQuote gnu.kawa.lispexpr.ReaderQuote)
(define-private-alias ReadTable gnu.kawa.lispexpr.ReadTable)
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
        (%bard-instance (Bard (bard-environment)))
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
;;; Bard class
;;; ---------------------------------------------------------------------

(define-simple-class Bard (Scheme)
  (instanceOf :: InstanceOf allocation: 'static access: 'public init-form: #!null)
  (notTrue :: Not allocation: 'static access: 'public init-form: #!null)
  (applyToArgsFn :: ApplyToArgs allocation: 'static access: 'public init-form: #!null)
  (applyFn :: Apply allocation: 'static access: 'public init-form: #!null)
  (identicalFn :: IsEq allocation: 'static access: 'public init-form: #!null)
  (equivalentFn :: IsEqv allocation: 'static access: 'public init-form: #!null)
  (equalFn :: IsEqual allocation: 'static access: 'public init-form: #!null)
  (numEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (isOddFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (isEvenFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (defaultPrompter :: Procedure allocation: 'static access: 'public init-form: default-prompter)
  (bardQuit :: Procedure allocation: 'static access: 'public init-form: bard-quit)
  ;; ---------------
  ;; primitive structures
  ;; ---------------
  (structureBigInteger::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "big-integer" gnu.math.IntNum
                                   (lambda (val)
                                     (if (eq? (val:getClass) gnu.math.IntNum)
                                         val
                                         (error (format #f "Not an integer: ~S" val))))))
  
  (structureBoolean::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "boolean" java.lang.Boolean
                                   (lambda (val) (if val #t #f))))
  
  (structureBox::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "box" Box
                                   (lambda (val)(Box val))))
  
  (structureClass::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "class" BardClass
                                   (lambda (name)(BardClass name))))
  
  (structureCons::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "cons" gnu.lists.Pair 
                                   (lambda (a b)(gnu.lists.Pair a b))))
  
  (structureDict::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "dict" HashPMap
                                   (lambda (#!rest args)(apply hashpmap args))))
  
  (structureDoubleFloat::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "double-float" gnu.math.DFloNum
                                   (lambda (val)
                                     (if (eq? (val:getClass) gnu.math.DFloNum)
                                         val
                                         (error (format #f "Not a double-float: ~S" val))))))
  
  (structureKeyword::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "keyword" gnu.expr.Keyword
                                   (lambda (val)
                                     (cond
                                      ((keyword? val) val)
                                      ((symbol? val) (string->keyword (symbol->string val)))
                                      ((string? val) (string->keyword val))
                                      (#t (error (format #f "Can't convert to a keyword: ~S"
                                                         val)))))))
  
  (structureRatio::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "ratio" gnu.math.IntFraction
                                   (lambda (val)
                                     (if (eq? (val:getClass) gnu.math.IntFraction)
                                         val
                                         (error (format #f "Not a ratio: ~S" val))))))
  
  (structureSequence::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "sequence" org.pcollections.ConsPStack
                                   (lambda (#!rest args)(org.pcollections.ConsPStack:from args))))
  
  (structureSingleFloat::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "single-float" java.lang.Float
                                   (lambda (val)
                                     (if (eq? (val:getClass) java.lang.Float)
                                         val
                                         (error (format #f "Not a single-float: ~S" val))))))
  
  (structureSymbol::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "symbol" gnu.mapping.Symbol
                                   (lambda (val)
                                     (cond
                                      ((keyword? val) (string->symbol (keyword->string val)))
                                      ((symbol? val) val)
                                      ((string? val) (string->symbol val))
                                      (#t (error (format #f "Can't convert to a symbol: ~S"
                                                         val)))))))
  
  (structureUnicodeChar::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "unicode-character" gnu.text.Char
                                   (lambda (val)
                                     (cond
                                      ((char? val) val)
                                      ((integer? val)(integer->char val))
                                      (#t (error (format #f "Not a character: ~S" val)))))))
  
  (structureUnicodeString::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "unicode-string" java.lang.String
                                   (lambda (#!rest args)(apply string args))))
  
  (structureURI::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "uri" URI (lambda (val)(URI val))))
  
  (structureVector::structure
   allocation: 'static access: 'public
   init-form: (primitive-structure "vector" gnu.lists.FVector
                                   (lambda (#!rest args)(apply vector args))))

  ;; Bard methods
  ;; ----------------------------------
  ((keywordsAreSelfEvaluating)(@java.lang.Override) ::boolean
   (begin #t))
  ((createReadTable)(@java.lang.Override) ::ReadTable
   (let* ((table::ReadTable (ReadTable:createInitial))
          (dispatchTable (as ReaderDispatch (*:lookup table (char->integer #\#))))
          (sentry (ReaderDispatchSyntaxQuote)))
     (*:set dispatchTable (char->integer #\') sentry)
     (*:set dispatchTable (char->integer #\`) sentry)
     (*:set dispatchTable (char->integer #\,) sentry)
     (*:putReaderCtorFld table "path" "gnu.kawa.lispexpr.LangObjType" "pathType")
     (*:putReaderCtorFld table "filepath" "gnu.kawa.lispexpr.LangObjType" "filepathType")
     (*:putReaderCtorFld table "URI" "gnu.kawa.lispexpr.LangObjType" "URIType")
     (*:putReaderCtor table "symbol" (ClassType:make "gnu.mapping.Symbol"))
     (*:putReaderCtor table "namespace" (ClassType:make "gnu.mapping.Namespace"))
     (*:putReaderCtorFld table "duration" "kawa.lib.numbers" "duration")
     (set! table:postfixLookupOperator #\:)
     (*:setFinalColonIsKeyword table #t)
     (*:set table (char->integer #\@)
            (ReaderQuote LispLanguage:splice_sym ReadTable:NON_TERMINATING_MACRO))
     table))

  ;; set up the bard runtime environment
  ;; ----------------------------------

  ((*init* env::Environment)
   (begin (invoke-special kawa.standard.Scheme (this) '*init* env)
          (Environment:setCurrent env)
          ;; ---------------
          ;; named constants
          ;; ---------------
          (*:defAliasStFld (this) "true" "java.lang.Boolean" "TRUE")
          (*:defAliasStFld (this) "false" "java.lang.Boolean" "FALSE")
          (*:defAliasStFld (this) "nothing" "gnu.lists.LList" "Empty")
          ;; ---------------
          ;; special forms
          ;; ---------------
          (*:defSntxStFld (this) "^" "kawa.standard.SchemeCompilation" "lambda")
          (*:defSntxStFld (this) "begin" "kawa.standard.begin" "begin")
          (*:defSntxStFld (this) "%define" "kawa.standard.define" "defineRaw")
          (*:defSntxStFld (this) "def" "kawa.lib.prim_syntax" "define")
          (*:defSntxStFld (this) "if" "kawa.lib.prim_syntax")
          (*:defSntxStFld (this) "quote" "kawa.lang.Quote" "plainQuote")
          ;; ---------------
          ;; built-in structures
          ;; ---------------
          (*:defProcStFld (this) "big-integer" "net.bardcode.Bard" "structureBigInteger")
          (*:defProcStFld (this) "boolean" "net.bardcode.Bard" "structureBoolean")
          (*:defProcStFld (this) "box" "net.bardcode.Bard" "structureBox")
          (*:defProcStFld (this) "class" "net.bardcode.Bard" "structureClass")
          (*:defProcStFld (this) "cons" "net.bardcode.Bard" "structureCons")
          (*:defProcStFld (this) "dict" "net.bardcode.Bard" "structureDict")
          (*:defProcStFld (this) "double-float" "net.bardcode.Bard" "structureDoubleFloat")
          (*:defProcStFld (this) "keyword" "net.bardcode.Bard" "structureKeyword")
          (*:defProcStFld (this) "ratio" "net.bardcode.Bard" "structureRatio")
          (*:defProcStFld (this) "sequence" "net.bardcode.Bard" "structureSequence")
          (*:defProcStFld (this) "single-float" "net.bardcode.Bard" "structureSingleFloat")
          (*:defProcStFld (this) "symbol" "net.bardcode.Bard" "structureSymbol")
          (*:defProcStFld (this) "unicode-character" "net.bardcode.Bard" "structureUnicodeChar")
          (*:defProcStFld (this) "unicode-string" "net.bardcode.Bard" "structureUnicodeString")
          (*:defProcStFld (this) "uri" "net.bardcode.Bard" "structureURI")
          (*:defProcStFld (this) "vector" "net.bardcode.Bard" "structureVector")
          ;; ---------------
          ;; built-in procedures
          ;; ---------------
          ;; repl prompt
          (*:defProcStFld (this) "default-prompter" "net.bardcode.Bard" "defaultPrompter")
          ;;;; quit
          (*:defProcStFld (this) "quit" "net.bardcode.Bard" "bardQuit")
          ;;;; instance?
          ;;;; TODO: make this work properly with structures; currently it works only
          ;;;;       with Java classes
          (begin (set! instanceOf (InstanceOf (this) "instance?"))
                 (*:defProcStFld (this) "instance?" "net.bardcode.Bard" "instanceOf"))
          ;; not
          (begin (set! notTrue (Not (this) "not"))
                 (*:defProcStFld (this) "not" "net.bardcode.Bard" "notTrue"))
          ;; apply
          (begin (set! applyToArgsFn (ApplyToArgs "apply-to-args" (this)))
                 (set! applyFn (gnu.kawa.functions.Apply "apply" applyToArgsFn))
                 (*:defProcStFld (this) "apply" "net.bardcode.Bard" "applyFn"))
          ;; identical?
          (begin (set! identicalFn (IsEq (this) "identical?"))
                 (*:defProcStFld (this) "identical?" "net.bardcode.Bard" "identicalFn"))
          ;; equivalent?
          (begin (set! equivalentFn (IsEqv (this) "equivalent?" identicalFn))
                 (*:defProcStFld (this) "equivalent?" "net.bardcode.Bard" "equivalentFn"))
          ;; equal?
          (begin (set! equalFn (IsEqual (this) "equal?"))
                 (*:defProcStFld (this) "equal?" "net.bardcode.Bard" "equalFn"))
          ;; =
          (begin (set! numEqFn (NumberCompare:make (this) "=" NumberCompare:TRUE_IF_EQU))
                 (*:defProcStFld (this) "=" "net.bardcode.Bard" "numEqFn"))
          ;; <
          (begin (set! numLessFn (NumberCompare:make (this) "<" NumberCompare:TRUE_IF_LSS))
                 (*:defProcStFld (this) "<" "net.bardcode.Bard" "numLessFn"))
          ;; <=
          (begin (set! numLessEqFn (NumberCompare:make
                                    (this) "<="
                                    (bitwise-ior NumberCompare:TRUE_IF_LSS
                                                 NumberCompare:TRUE_IF_EQU)))
                 (*:defProcStFld (this) "<=" "net.bardcode.Bard" "numLessEqFn"))
          ;; >
          (begin (set! numGreaterFn (NumberCompare:make (this) ">" NumberCompare:TRUE_IF_GRT))
                 (*:defProcStFld (this) ">" "net.bardcode.Bard" "numGreaterFn"))
          ;; >=
          (begin (set! numGreaterEqFn (NumberCompare:make
                                       (this) ">="
                                       (bitwise-ior NumberCompare:TRUE_IF_GRT
                                                    NumberCompare:TRUE_IF_EQU)))
                 (*:defProcStFld (this) ">=" "net.bardcode.Bard" "numGreaterEqFn"))
          ;; odd?
          (begin (set! isOddFn (NumberPredicate (this) "odd?" NumberPredicate:ODD))
                 (*:defProcStFld (this) "odd?" "net.bardcode.Bard" "isOddFn"))
          ;; even?
          (begin (set! isEvenFn (NumberPredicate (this) "even?" NumberPredicate:EVEN))
                 (*:defProcStFld (this) "even?" "net.bardcode.Bard" "isEvenFn"))))

  ;; ----------------------------------
  ;; methods
  ;; ----------------------------------

  ((getInstance) allocation: 'static (bard-instance))
  ((builtin) allocation: 'static (bard-environment)))

