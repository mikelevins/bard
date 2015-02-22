;;; Definitions for some standard syntax.

(module-export cond and or let let* do delay lazy delay-force
               else ... => _ else unquote unquote-splicing
               syntax->datum datum->syntax with-syntax
	       syntax-object->datum datum->syntax-object ; deprecated
	       generate-temporaries define-procedure
	       identifier? free-identifier=? bound-identifier=?
	       syntax-source syntax-line syntax-column)

(require <kawa.lib.prim_syntax>)

(import (rename (only (kawa standard let) let) (let %let)))
(import (rename (only (kawa standard define) defineRaw) (defineRaw %define)))
(import (only (kawa standard SchemeCompilation) lambda))
(import (only (kawa standard Scheme) not))
(import (only (kawa standard begin) begin))

(define-syntax =>
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of '=>"))))

(define-syntax ...
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of '..."))))

(define-syntax _
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of '_"))))

(define-syntax else
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of 'else"))))

(define-syntax unquote
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of 'unquote"))))

(define-syntax unquote-splicing
  (syntax-rules ()
    ((_ . rest)
     (syntax-error "invalid use of 'unquote-splicing"))))

;;; COND

(define-syntax cond
  (syntax-rules (else =>)

		((cond (else result1 result2 ...))
		 (begin result1 result2 ...))

		((cond (else result1 result2 ...) clause1 ...)
		 (syntax-error "else clause must be last clause of cond"))

		((cond (test => result))
		 (%let ((temp test))
		   (if temp (result temp))))

		((cond (test => result) clause1 clause2 ...)
		 (%let ((temp test))
		   (if temp
		       (result temp)
		       (cond clause1 clause2 ...))))

		((cond (test))
		 test)

		((cond (test) clause1 clause2 ...)
		 (or test (cond clause1 clause2 ...)))

		((cond (test result1 result2 ...))
		 (if test (begin result1 result2 ...)))

		((cond (test result1 result2 ...) clause1 clause2 ...)
		 (if test
		     (begin result1 result2 ...)
		     (cond clause1 clause2 ...)))))

(define-syntax %lang-boolean
  (syntax-rules ()
    ((%lang-boolean value)
     (make <gnu.expr.QuoteExp>
       (invoke
	(gnu.expr.Language:getDefaultLanguage)
	'booleanObject value)))))

;;; AND

(define-syntax (and f)
  (syntax-case f ()
	       ((_) (%lang-boolean #t))
	       ((_ test) (syntax test))
	       ((_ test1 . test2)
		#`(%let ((x test1))
			  (if x (and . test2) x)))))
;;; OR

(define-syntax (or f)
  (syntax-case f ()
	       ((_) (%lang-boolean #f))
	       ((_ test) (syntax test))
	       ((_ test1 test2 ...)
		(syntax (%let ((x test1))
			  (if x x (or test2 ...)))))))

;;; LET (including named let)

(define-syntax %let-lambda1
  (syntax-rules (::)
		((%let-lambda1 ((var type init) . in) out body)
		 (%let-lambda1 in ((var type) . out) body))
		((%let-lambda1 ((var :: type init) . in) out body)
		 (%let-lambda1 in ((var type) . out) body))
		((%let-lambda1 ((var init) . in) out body)
		 (%let-lambda1 in (var . out) body))
		((%let-lambda1 () out body)
		 (%let-lambda2 out () body))))

;;; This is just to reverse the argument list yielded by %let-lambda1.
(define-syntax %let-lambda2
  (syntax-rules ()
		((%let-lambda2 (arg . in) out body)
		 (%let-lambda2 in (arg . out) body))
		((%let-lambda2 () out body)
		 (lambda out . body))))

(define-syntax %let-init
  (syntax-rules (::)
		((%let-init (var init))
		 init)
		((%let-init (var :: type init))
		 init)
		((%let-init (var type init))
		 init)
		((%let-init (var))
		 (syntax-error "let binding with no value"))
		((%let-init (var a b c))
		 (syntax-error
		  "let binding must have syntax: (var [type] init)"))))

(define-syntax let
  (syntax-rules ()
    ((let (binding ...) . body)
     ;; We destructure the bindings to simplify %let's job.
     (%let (binding ...) . body))
    ;; Alternative definition would be simpler, but makes more work for
    ;; the compiler to optimize it - and still doesn't do quite as well.
    ;;((%let-lambda1 (binding ...) () body)
    ;;(%let-init binding) ...))
    ((let name (binding ...) . body)
     ((letrec ((name (%let-lambda1 (binding ...) () body)))
	name)
      (%let-init binding) ...))))

;;; LET*

(define-syntax let*
  (syntax-rules ()
    ((let* () . body) (%let () . body))
    ((let* (var-init) . body)
     (%let (var-init) . body))
    ((let* (var-init . bindings) . body)
     (%let (var-init)
	   (let* bindings . body)))
    ((let* bindings . body)
     (syntax-error
      "invalid bindings list in let*"))
    ((let* . body)
     (syntax-error
      "missing bindings list in let*"))))

;;; DO

;;; Helper macro for do, to handle optional step.
(define-syntax %do-step
  (syntax-rules (::)
		((%do-step variable :: type init step) step)
		((%do-step variable :: type init) variable)
		((%do-step variable init step) step)
		((%do-step variable init) variable)))

(define-syntax %do-init
  (syntax-rules (::)
		((%do-init (var :: type init step))
		 init)
		((%do-init (var :: type init))
		 init)
		((%do-init (var init step))
		 init)
		((%do-init (var init))
		 init)
		((%do-init (var type init))
		 init)
		((%do-init (var))
		 (syntax-error "do binding with no value"))
		((%do-init (var a b c))
		 (syntax-error
		  "do binding must have syntax: (var [:: type] init [step])"))))

(define-syntax %do-lambda1
  (syntax-rules (::)
		((%do-lambda1 ((var :: type init step) . in) out body)
		 (%do-lambda1 in ((var :: type) . out) body))
		((%do-lambda1 ((var :: type init) . in) out body)
		 (%do-lambda1 in ((var :: type) . out) body))
		((%do-lambda1 ((var init step) . in) out body)
		 (%do-lambda1 in (var . out) body))
		((%do-lambda1 ((var init) . in) out body)
		 (%do-lambda1 in (var . out) body))
		((%do-lambda1 () out body)
		 (%do-lambda2 out () body))))

;;; This is just to reverse the argument list yielded by %do-lambda1.
(define-syntax %do-lambda2
  (syntax-rules ()
		((%do-lambda2 (arg . in) out body)
		 (%do-lambda2 in (arg . out) body))
		((%do-lambda2 () out body)
		 (lambda out body))))

(define-syntax do
  (syntax-rules (::)
		((do (binding ...)
		     (test . result) commands ...)
		 ;; The identifier %do%loop is optimized specially ...
		 (letrec ((%do%loop
			   (%do-lambda1 (binding ...) ()
					;; Use a 'not' so the exit clause is
					;; at the end, which avoids a goto.
					(if (not test)
					    (begin commands ...
						   (%do%loop (%do-step . binding) ...))
					    (begin #!void . result)))))
		   (%do%loop (%do-init binding) ...)))))

;;; DELAY

;; See racket-5.2/collects/racket/private/promise.rkt
;; See SRFI-45

(define-syntax (lazy form)
  (syntax-case form ()
    ((_ expression)
     (gnu.expr.ApplyExp gnu.kawa.functions.MakePromise:makeLazy
                        (syntax->expression #'(lambda () expression))))))

(define-syntax (delay-force form)
  (syntax-case form ()
    ((_ expression)
     (gnu.expr.ApplyExp gnu.kawa.functions.MakePromise:makeLazy
                        (syntax->expression #'(lambda () expression))))))

(define-syntax (delay form)
  (syntax-case form ()
    ((delay expression)
     (gnu.expr.ApplyExp gnu.kawa.functions.MakePromise:makeDelay
                        (syntax->expression #'(lambda () expression))))))

(define-syntax define-procedure
   (syntax-rules ()
     ((_ name . rest)
      (%define name 27 gnu.expr.GenericProc . rest))))

(define (syntax->datum obj)
  (kawa.lang.Quote:quote obj))

(define (syntax-object->datum obj)
  (syntax->datum obj))

(define (datum->syntax template-identifier obj #!optional (srcloc #!null))
  (kawa.lang.SyntaxForms:makeWithTemplate template-identifier obj srcloc))

(define (datum->syntax-object template-identifier obj)
  (datum->syntax template-identifier obj))

(define (generate-temporaries list)
  (let loop ((n (kawa.lang.Translator:listLength list)) (lst '()))
    (if (= n 0) lst
	(loop (- n 1) (make <pair> (datum->syntax-object list (gnu.expr.Symbols:gentemp)) lst)))))

(define (identifier? form) :: <boolean>
  (or (gnu.mapping.Symbol? form)
      (and (kawa.lang.SyntaxForm? form)
	   (kawa.lang.SyntaxForms:isIdentifier form))))

(define (free-identifier=? id1 id2) :: <boolean>
  (if (and (identifier? id1) (identifier? id2))
      (kawa.lang.SyntaxForms:identifierEquals id1 id2 #f)
      (report-syntax-error
       (if (identifier? id1) id2 id1)
       "free-identifier-? - argument is not an identifier")))

(define (bound-identifier=? id1 id2) :: <boolean>
  (if (and (identifier? id1) (identifier? id2))
      (kawa.lang.SyntaxForms:identifierEquals id1 id2 #t)
      (report-syntax-error
       (if (identifier? id1) id2 id1)
       "bound-identifier-? - argument is not an identifier")))

(define (syntax-source form)
  (cond ((instance? form kawa.lang.SyntaxForm)
	 (syntax-source (as kawa.lang.SyntaxForm form):datum))
	((instance? form gnu.lists.PairWithPosition)
	 (%let ((str (*:getFileName (as  gnu.lists.PairWithPosition form))))
	   (if (eq? str #!null) #f  str)))
	(else
	 #f)))

(define (syntax-line form)
  (cond ((instance? form kawa.lang.SyntaxForm)
	 (syntax-line (as kawa.lang.SyntaxForm form):datum))
	((instance? form gnu.lists.PairWithPosition)
	 ((as gnu.lists.PairWithPosition form):getLineNumber))
	(else
	 #f)))

;; zero-origin for compatility with MzScheme.
(define (syntax-column form)
  (cond ((instance? form <kawa.lang.SyntaxForm>)
	 (syntax-line (as kawa.lang.SyntaxForm form):datum))
	((instance? form gnu.lists.PairWithPosition)
	 (- ((as gnu.lists.PairWithPosition form):getColumnNumber) 0))
	(else
	 #f)))

;;; The definition of include is based on that in the portable implementation
;;; of syntax-case psyntax.ss, whixh is again based on Chez Scheme.
;;; Copyright (c) 1992-2002 Cadence Research Systems
;;; Permission to copy this software, in whole or in part, to use this
;;; software for any lawful purpose, and to redistribute this software
;;; is granted subject to the restriction that all copies made of this
;;; software must include this copyright notice in full.  This software
;;; is provided AS IS, with NO WARRANTY, EITHER EXPRESS OR IMPLIED,
;;; INCLUDING BUT NOT LIMITED TO IMPLIED WARRANTIES OF MERCHANTABILITY
;;; OR FITNESS FOR ANY PARTICULAR PURPOSE.  IN NO EVENT SHALL THE
;;; AUTHORS BE LIABLE FOR CONSEQUENTIAL OR INCIDENTAL DAMAGES OF ANY
;;; NATURE WHATSOEVER.

;; Converted to use syntax-rules from the psyntax.ss implementation.

(define-syntax with-syntax
   (syntax-rules ()
     ((with-syntax () e1 e2 ...)
      (begin e1 e2 ...))
     ((with-syntax ((out in)) e1 e2 ...)
      (syntax-case in () (out (begin e1 e2 ...))))
     ((with-syntax ((out in) ...) e1 e2 ...)
      (syntax-case (list in ...) ()
		   ((out ...) (begin e1 e2 ...))))))
