;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       bard interpreter in Scheme
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; gambit prerequisutes
;;;---------------------------------------------------------------------

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")

;;;---------------------------------------------------------------------
;;; general utilities
;;;---------------------------------------------------------------------

(define (plist->table . k/v-plist)
  (let ((tbl (make-table test: eq?)))
    (let loop ((kvs k/v-plist))
      (if (null? kvs)
          tbl
          (if (null? (cdr kvs))
              (error "odd number of inits in make-table" k/v-plist)
              (begin
                (table-set! tbl (car kvs)(cadr kvs))
                (loop (cddr kvs))))))))

(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

;;;---------------------------------------------------------------------
;;; bard values
;;;---------------------------------------------------------------------

(define (bard:value->type thing)
  (cond 
   ((eq? thing #!void) 'undefined)
   ((null? thing) 'null)
   ((eq? thing #t) 'true)
   ((eq? thing #f) 'false)
   ((symbol? thing) 'symbol)
   ((keyword? thing) 'keyword)
   ((integer? thing) 'integer)
   ((flonum? thing) 'float)
   ((##ratnum? thing) 'ratio)
   ((pair? thing) 'pair)
   (else 'unknown)))

(define (undefined) #!void)
(define (nothing) '())
(define (true) #t)
(define (false) #f)

;;;---------------------------------------------------------------------
;;; bard reader
;;;---------------------------------------------------------------------

(define $bard-readtable
  (let ((rt (##make-standard-readtable)))
    (macro-readtable-keywords-allowed?-set! rt 'prefix)
    rt))

(define (%string->value s)
  (call-with-input-string
   (list init: s
         readtable: $bard-readtable)
   read))

(define (%read-value->bard-value r)
  r)

(define (bard:read-from-string s)
  (%read-value->bard-value (%string->value s)))

;;;---------------------------------------------------------------------
;;; bard primitives
;;;---------------------------------------------------------------------

(define-type bard:prim
  id: 3520C851-B065-48DA-80B9-358367AF8A3E
  constructor: bard:%make-prim
  (name bard:%prim-name)
  (argcount bard:%prim-argcount)
  (fun bard:%prim-fun))

(define (bard:make-prim name arg-count fun)
  (bard:%make-prim name arg-count fun))

(define $bard-primitives (make-table test: eq?))

(define (bard:defprim name argcount primfun)
  (table-set! $bard-primitives name (bard:make-prim name argcount primfun)))

(bard:defprim '+ #f (lambda args (apply + args)))
(bard:defprim '- #f (lambda args (apply - args)))
(bard:defprim '* #f (lambda args (apply * args)))
(bard:defprim '/ #f (lambda args (apply / args)))
(bard:defprim '> #f (lambda args (apply > args)))
(bard:defprim '< #f (lambda args (apply < args)))

;;;---------------------------------------------------------------------
;;; special forms
;;;---------------------------------------------------------------------

(define (bard:eval-define exp env)
  (not-yet-implemented))

(define (bard:eval-if exp env)
  (not-yet-implemented))

(define (bard:eval-method exp env)
  (not-yet-implemented))

(define $special-form-table
  (plist->table
   'define bard:eval-define
   'if bard:eval-if
   'method bard:eval-method))

(define (bard:lookup-printer x)
  (table-ref $printer-table x))

(define (bard:value->printer thing)
  (bard:lookup-printer (bard:value->type thing)))

(define (bard:print thing)
  (let ((printer (bard:value->printer thing)))
    (printer thing)))

;;;---------------------------------------------------------------------
;;; bard evaluator
;;;---------------------------------------------------------------------

(define (bard:make-initial-environment)
  (let ((env '()))
    (table-for-each (lambda (k v)
                      (set! env
                            (cons (cons k v) env)))
                    $bard-primitives)
    (box env)))

(define (%find-entry env var)
  (assq var (unbox env)))

(define (bard:lookup var env)
  (let ((ent (%find-entry (unbox env) var)))
    (if ent (cdr ent) #!void)))

(define (bard:add-variable env var val)
  (box (cons (cons var val) (unbox env))))

(define $bard-initial-environment
  (bard:make-initial-environment))

(define (bard:operator->type op)
  (cond
   ((bard:prim? op) 'primitive)
   (else 'unknown)))

(define (bard:apply op args)
  (let ((op-type (bard:operator->type op)))
    (case op-type
      ((primitive)(apply (bard:%prim-fun op) args))
      (else (error "Invalid operator" op)))))

(define (special-form? symbol)
  (not-yet-implemented 'special-form?))

(define (macro-name? symbol)
  (not-yet-implemented 'macro-name?))

(define (bard:eval-special-form exp env)
  (not-yet-implemented 'bard:eval-special-form))

(define (bard:eval-macro-form exp env)
  (not-yet-implemented 'bard:eval-macro-form))

(define (bard:eval exp env)
  (let ((exp-type (bard:value->type exp)))
    (case exp-type
      ((undefined null true false keyword integer float ratio) exp)
      ((symbol)(bard:lookup exp env))
      ((pair)(cond
              ((null? exp)(nothing))
              ((special-form? (car exp))(bard:eval-special-form exp env))
              ((macro-name? (car exp))(bard:eval-macro-form exp env))
              (else (let ((op (bard:eval (car exp) env))
                          (args (map (lambda (x)(bard:eval x env))
                                     (cdr exp))))
                      (bard:apply op args)))))
      (else (error "Invalid expression syntax" exp)))))

;;;---------------------------------------------------------------------
;;; bard printer
;;;---------------------------------------------------------------------

(define (bard:print-undefined thing)
  (display "undefined"))

(define (bard:print-null thing)
  (display "nothing"))

(define (bard:print-boolean thing)
  (if thing (display "true")(display "false")))

(define (bard:print-symbol thing)
  (display thing))

(define (bard:print-keyword thing)
  (let ((s (string-append ":" (keyword->string thing))))
    (display s)))

(define (bard:print-integer thing)
  (display thing))

(define (bard:print-float thing)
  (display thing))

(define (bard:print-ratio thing)
  (display thing))

(define (bard:print-pair thing)
  (display thing))

(define (bard:print-unknown thing)
  (display "#<unknown value type: ")
  (display thing)
  (display ">"))

(define $printer-table
  (plist->table
   'undefined bard:print-undefined
   'null bard:print-null
   'true bard:print-boolean
   'false bard:print-boolean
   'symbol bard:print-symbol
   'keyword bard:print-keyword
   'integer bard:print-integer
   'float bard:print-float
   'ratio bard:print-ratio
   'pair bard:print-pair
   'unknown bard:print-unknown))

(define (bard:lookup-printer x)
  (table-ref $printer-table x))

(define (bard:value->printer thing)
  (bard:lookup-printer (bard:value->type thing)))

(define (bard:print thing)
  (let ((printer (bard:value->printer thing)))
    (printer thing)))

;;;---------------------------------------------------------------------
;;; bard repl
;;;---------------------------------------------------------------------

(define $banner "Bard 0.1\n")

(define $prompt "\n? ")

(define (bard-repl)
  (display $banner)
  (display $prompt)
  (let ((env $bard-initial-environment))
    (let loop ((input (bard:read-from-string (read-line))))
      (if (eq? input #!eof)
          (loop (bard:read-from-string (read-line)))
          (if (eq? input q:)
              (display "\nBard terminated\n")
              (let ((val (bard:eval input env)))
                (bard:print val)
                (display $prompt)
                (loop (bard:read-from-string (read-line)))))))))
