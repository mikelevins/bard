;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          language.scm
;;;; Project:       bard 0.4
;;;; Purpose:       language definition
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export
 BardLanguage
 bard-environment
 bard-instance
 null-environment
 )

;;; ---------------------------------------------------------------------
;;; Java imports
;;; ---------------------------------------------------------------------

(define-private-alias Apply gnu.kawa.functions.Apply)
(define-private-alias ApplyToArgs gnu.kawa.functions.ApplyToArgs)
(define-private-alias ClassType gnu.bytecode.ClassType)
(define-private-alias Environment gnu.mapping.Environment)
(define-private-alias InstanceOf gnu.kawa.reflect.InstanceOf)
(define-private-alias IsEq gnu.kawa.functions.IsEq)
(define-private-alias IsEqual gnu.kawa.functions.IsEqual)
(define-private-alias IsEqv gnu.kawa.functions.IsEqv)
(define-private-alias LispLanguage gnu.kawa.lispexpr.LispLanguage)
(define-private-alias Map gnu.kawa.functions.Map)
(define-private-alias Not gnu.kawa.functions.Not)
(define-private-alias NumberCompare gnu.kawa.functions.NumberCompare)
(define-private-alias NumberPredicate gnu.kawa.functions.NumberPredicate)
(define-private-alias Procedure gnu.mapping.Procedure)
(define-private-alias ReadTable gnu.kawa.lispexpr.ReadTable)
(define-private-alias ReaderDispatch gnu.kawa.lispexpr.ReaderDispatch)
(define-private-alias ReaderDispatchSyntaxQuote gnu.kawa.lispexpr.ReaderDispatchSyntaxQuote)
(define-private-alias ReaderQuote gnu.kawa.lispexpr.ReaderQuote)
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
  (numEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numLessEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (numGreaterEqFn :: NumberCompare allocation: 'static access: 'public init-form: #!null)
  (isOddFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (isEvenFn :: NumberPredicate allocation: 'static access: 'public init-form: #!null)
  (defaultPrompter :: Procedure allocation: 'static access: 'public init-form: default-prompter)

  ;; Bard methods
  ;; ----------------------------------
  ((keywordsAreSelfEvaluating)(@java.lang.Override) ::boolean (begin #t))
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
          ;; named constants
          ;; ---------------
          (*:defAliasStFld (this) "true" "java.lang.Boolean" "TRUE")
          (*:defAliasStFld (this) "false" "java.lang.Boolean" "FALSE")
          (*:defAliasStFld (this) "nothing" "gnu.lists.LList" "Empty")
          ;; special forms
          ;; ---------------
          (*:defSntxStFld (this) "begin" "kawa.standard.begin" "begin")
          (*:defSntxStFld (this) "if" "kawa.lib.prim_syntax")
          (*:defSntxStFld (this) "method" "kawa.standard.SchemeCompilation" "lambda")
          (*:defSntxStFld (this) "quote" "kawa.lang.Quote" "plainQuote")
          ))

  ;; ----------------------------------
  ;; methods
  ;; ----------------------------------

  ((getInstance) allocation: 'static (bard-instance))
  ((builtin) allocation: 'static (bard-environment)))

