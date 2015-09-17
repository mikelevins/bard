(module-export defmacro define-macro define-syntax-case
	       begin-for-syntax define-for-syntax
               when unless try-finally synchronized
               let-values let*-values case-lambda define-values
               receive define-alias-parameter
               $string$ $string-with-default-format$ $format$ $sprintf$
               $string-with-delimiter-marks$ define-simple-constructor)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)
(require <kawa.lib.lists>)
(require <kawa.lib.scheme.eval>)
(require <kawa.lib.prim_imports>)

(define-syntax defmacro
  (syntax-rules ()
		((defmacro name pattern . forms)
		 (%define-macro name (lambda pattern . forms)))))

(define-syntax define-macro
  (syntax-rules ()
    ((define-macro (name . pattern) . forms)
     (%define-macro name (lambda pattern . forms)))
    ((define-macro name function)
     (%define-macro name function))))

(define-syntax define-syntax-case
   (syntax-rules ()
     ((define-syntax-case name literals . parts)
      (define-syntax name
        (lambda (form)
          (syntax-case form literals . parts))))))

(define-syntax when (syntax-rules ()
				  ((when cond exp ...)
				   (if cond (begin exp ...)))))

(define-syntax unless (syntax-rules ()
				  ((when cond exp ...)
				   (if (not cond) (begin exp ...)))))

(define-rewrite-syntax try-finally
  (lambda (x)
    (syntax-case x ()
      ((_ try-part finally-part)
       (make <gnu.expr.TryExp>
         (syntax->expression (syntax try-part))
         (syntax->expression (syntax finally-part)))))))

(define-rewrite-syntax synchronized
  (lambda (x)
    (syntax-case x ()
      ((_ object . body)
       (make <gnu.expr.SynchronizedExp>
         (syntax->expression (syntax object))
         (syntax-body->expression (syntax body)))))))


(define-syntax begin-for-syntax
  (lambda (form)
    (syntax-case form ()
      ((begin-for-syntax . body)
       (eval (syntax-object->datum (gnu.lists.Pair 'begin (syntax body))))
       (syntax #!void)))))

(define-syntax define-for-syntax
  (syntax-rules ()
    ((define-for-syntax . rest)
     (begin-for-syntax
      (define . rest)))))

;; LET-VALUES implementation from SRFI-11, by Lars T Hansen.
;; http://srfi.schemers.org/srfi-11/srfi-11.html

;; This code is in the public domain.

(define-syntax let-values
  (syntax-rules ()
    ((let-values (?binding ...) ?body0 ?body1 ...)
     (let-values "bind" (?binding ...) () (begin ?body0 ?body1 ...)))
    ((let-values "bind" () ?tmps ?body)
     (let ?tmps ?body))
    
    ((let-values "bind" ((?b0 ?e0) ?binding ...) ?tmps ?body)
     (let-values "mktmp" ?b0 ?e0 () (?binding ...) ?tmps ?body))
    
    ((let-values "mktmp" () ?e0 ?args ?bindings ?tmps ?body)
     (call-with-values 
       (lambda () ?e0)
       (lambda ?args
         (let-values "bind" ?bindings ?tmps ?body))))
    
    ((let-values "mktmp" (?a . ?b) ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (let-values "mktmp" ?b ?e0 (?arg ... x) ?bindings (?tmp ... (?a x)) ?body))
    
    ((let-values "mktmp" ?a ?e0 (?arg ...) ?bindings (?tmp ...) ?body)
     (call-with-values
       (lambda () ?e0)
       (lambda (?arg ... . x)
         (let-values "bind" ?bindings (?tmp ... (?a x)) ?body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () ?body0 ?body1 ...)
     (begin ?body0 ?body1 ...))

    ((let*-values (?binding0 ?binding1 ...) ?body0 ?body1 ...)
     (let-values (?binding0)
        (let*-values (?binding1 ...) ?body0 ?body1 ...)))))

(define-syntax (case-lambda form)
  (syntax-case form ()
    ((_ . cl)
     #`(gnu.expr.GenericProc:makeWithoutSorting
	. #,(let loop ((clauses #'cl))
	     (syntax-case clauses ()
	       (((formals . body) . rest)
		#`((lambda formals . body) . #,(loop #'rest)))
	       (()
		'())
	       (rest
		(list (report-syntax-error (syntax rest)
				    "invalid case-lambda clause")))))))))

;; Implementation copied from R7RS.
(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
         (lambda args #f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
           (define varn
             (let ((v (cadr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
           list))
       (define var1
         (let ((v (cadr var0)))
           (set-cdr! var0 (cddr var0))
           v)) ...
           (define varn
             (let ((v (cdr var0)))
               (set! var0 (car var0))
               v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr)
         list)))))

;; RECEIVE implementation from http://srfi.schemers.org/srfi-8/srfi-8.html
;; Copyright (C) John David Stone (1999). All Rights Reserved.
;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

(define-syntax define-alias-parameter
  (syntax-rules ()
    ((define-alias-parameter name type location)
     (begin
       (define-constant name :: <gnu.mapping.LocationProc>
	 (gnu.mapping.LocationProc:makeNamed 'name location))
       (gnu.mapping.LocationProc:pushConverter
	name
	(lambda (arg)
	  (try-catch
	   (as type arg)
	   (ex <java.lang.ClassCastException>
	       (let ((wt (gnu.mapping.WrongType:make ex name
						     (as <int> 1) arg)))
		 (set! (field wt 'expectedType) type)
		 (primitive-throw wt))))))))))

;; Helper macros for $string$:
;; Collect format string (assuming we're *not* inside $<<$ ... $>>$)
;; Returns list of format string fragments, to be concatenated.
(define (%string-format-format default-format forms)
  (syntax-case forms ($format$ $<<$ $>>$)
    (() '())
    (($<<$ . rest)
     (%string-format-enclosed-format default-format #'rest))
    ((($format$ fstr . args) . rest)
     (cons #'fstr (%string-format-format default-format #'rest)))
    (((x . y) . rest)
      (cons default-format (%string-format-format default-format #'rest)))
    ((x . rest)
     (cons #'(constant-fold invoke (constant-fold invoke x 'toString)
                            'replace "~" "~~")
           (%string-format-format default-format #'rest)))))

;; Collect format string, assuming we're inside $<<$ ... $>>$
;; Returns list of format string fragments, to be concatenated.
(define (%string-format-enclosed-format default-format forms)
  (syntax-case forms ($<<$ $>>$ $splice$)
    (() '())
    (($>>$ . rest)
     (%string-format-format default-format #'rest))
    ((($splice$ seq) . rest)
     (cons #`(constant-fold string-append "~{" #,default-format "~}")
           (%string-format-enclosed-format default-format #'rest)))
    ((arg1 . rest)
     (cons default-format (%string-format-enclosed-format default-format #'rest)))))

;; Collect format arguments (assuming we're *not* inside $<<$ ... $>>$)
(define (%string-format-args forms)
  (syntax-case forms ($format$ $<<$ $>>$)
    (() '())
    (($<<$ . rest)
     (%string-format-enclosed-args #'rest))
    ((($format$ fstr arg ...) . rest)
     #`(arg ... . #,(%string-format-args #'rest)))
    (((x . y) . rest)
     #`((x . y) . #,(%string-format-args #'rest)))
    ((x . rest)
     (%string-format-args #'rest))))
                         
;; Collect format arguments, assuming we're inside $<<$ ... $>>$
(define (%string-format-enclosed-args forms)
  (syntax-case forms ($format$ $splice$ $<<$ $>>$)
    (() '())
    (($>>$ . rest)
     (%string-format-args #'rest))
    ((($splice$ seq) . rest)
     #`(seq . #,(%string-format-enclosed-args #'rest)))
    ((arg . rest)
     #`(arg . #,(%string-format-enclosed-args #'rest)))))

(define-syntax $string-with-default-format$
  (lambda (form)
    (syntax-case form ()
      (($string$ default-format . forms)
       #`($format$ (constant-fold invoke
                                  (constant-fold string-append
                                                 . #,(%string-format-format
                                                      #'default-format
                                                      #'forms))
                                  'toString)
                   . #,(%string-format-args #'forms))))))

(define-syntax $string$
  (syntax-rules ()
    ((_ . args)
     ($string-with-default-format$ "~a" . args))))

(define-syntax $string-with-delimiter-marks$
  (syntax-rules ()
    ((_ . args)
     ($string-with-default-format$ "~Q" . args))))

(define-syntax $format$
  (syntax-rules ()
    ((_ . args)
     (gnu.kawa.functions.Format:formatToString 0 . args))))

(define-syntax $sprintf$
  (syntax-rules ()
    ((_ fmt . args)
     (gnu.kawa.functions.Format:sprintfToString fmt . args))))

(cond-expand
 (kawa
  ;; (%symbol->construct 'sym) returns '$construct$:sym
  ;; In Kawa we lookup sym in the $construct$ package.
  (define-syntax %symbol->construct
    (syntax-rules ()
      ((%symbol->construct sym)
       (symbol sym $construct$)))))
 (else
  ;; In a Scheme without namespaces (i.e. colon not special)
  ;; construct a regular symbol.
  (define-syntax %symbol->construct
    (syntax-rules ()
      ((%symbol->construct sym)
       (string->symbol (string-append "$construct$:"
                                      (symbol->string sym))))))))

(define-syntax define-simple-constructor
  (lambda (form)
    (syntax-case form ($construct$)
      ((_ name constructor text-collector)
       (let ((cname (datum->syntax #'name (%symbol->construct (syntax->datum #'name)))))
         #`(define-syntax #,cname
             (syntax-rules ()
               ((#,cname . args)
                (%simple-construct-builder constructor text-collector () . args))))))
      ((_ name constructor)
       #`(define-simple-constructor name constructor $string$)))))

(define-syntax %simple-construct-builder
  (syntax-rules ($<<$ $>>$)
    ((%simple-construct-builder fun mkstr (seen ...) $<<$ . rest)
     (fun (mkstr seen ... $<<$ . rest)))
    ((%simple-construct-builder fun mkstr (seen ...) $>>$ . rest)
     (fun seen ... (mkstr . rest)))
    ((%simple-construct-builder fun mkstr (seen ...) x . rest)
     (%simple-construct-builder fun mkstr (seen ... x) . rest))
    ((%simple-construct-builder fun mkstr (seen ...))
     (fun (mkstr seen ... )))))
