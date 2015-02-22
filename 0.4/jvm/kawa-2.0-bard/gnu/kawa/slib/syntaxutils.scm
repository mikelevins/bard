;; syntaxexpand.scm --- Expand macros in S-exps.
;;
;; This code was written by Helmut Eller and has been placed in the
;; public domain.

(module-export expand)

(module-static #t)

(module-compile-options
 warn-invoke-unknown-method: #t
 warn-undefined-variable: #t
 )

(define-syntax typecase%
  (syntax-rules (eql or)
    ((typecase% var (#t body ...) more ...)
     (begin body ...))
    ((typecase% var ((eql value) body ...) more ...)
     (cond ((eqv? var 'value) body ...)
           (else (typecase% var more ...))))
    ((typecase% var ((or type) body ...) more ...)
     (typecase% var (type body ...) more ...))
    ((typecase% var ((or type ...) body ...) more ...)
     (let ((f (lambda (var) (begin body ...))))
       (typecase% var
                  (type (f var)) ...
                  (#t (typecase% var more ...)))))
    ((typecase% var (type body ...) more ...) 
     (cond ((instance? var type) 
            (let ((var :: type var))
              (begin body ...)))
           (else (typecase% var more ...))))
    ((typecase% var)
     (error "typecase% failed" var 
            (! getClass (as <object> var))))))

;; Roughly similar to CL's typecase.
(define-syntax-case typecase
    ()
  ((_ exp more ...) (identifier? (syntax exp))
   #`(typecase% exp more ...))
  ((_ exp more ...)
   #`(let ((tmp exp))
       (typecase% tmp more ...))))

(define-syntax !
  (syntax-rules ()
    ((! name obj args ...)
     (invoke obj 'name args ...))))

(define-syntax |@|
  (syntax-rules ()
    ((|@| name obj)
     (field obj 'name))))

(define-syntax packing
  (syntax-rules ()
    ((packing (var) body ...)
     (let ((var :: <list> '()))
       (let ((var (lambda (v) (set! var (cons v var)))))
         body ...)
       (reverse! var)))))


;; Take a S-expression and return a Sexp with all macros expanded.
(define (expand sexp #!key (env (interaction-environment)))
  (unrewrite (rewrite-form `(begin ,sexp) env: env)))

;; Take a Sexp and return the expanded Expression tree.
(define (rewrite-form exp #!key 
		      (lang ::gnu.expr.Language (gnu.expr.Language:getDefaultLanguage))
		      (env (interaction-environment))) :: <gnu.expr.Expression>
  (define-alias C <gnu.expr.Compilation>)
  (let* ((namelookup (<gnu.expr.NameLookup>:getInstance env lang))
	 (messages (<gnu.text.SourceMessages>))
	 (translator ::kawa.lang.Translator
                     (lang:getCompilation messages namelookup))
	 (module (! pushNewModule translator (as <String> #!null)))
	 (saved-comp (C:set-save-current translator)))
    (try-finally
     (! rewrite translator exp)
     (C:restore-current saved-comp))))

;; Given an Expresssion try to reconstruct the corresponding Sexp.
(define (unrewrite (exp <gnu.expr.Expression>))
  (typecase exp
    (<gnu.expr.LetExp> (unrewrite-let exp))
    (<gnu.expr.QuoteExp> (unrewrite-quote exp))
    (<gnu.expr.SetExp> 
     `(set ,(! get-symbol exp) ,(unrewrite (! get-new-value exp))))
    (<gnu.expr.LambdaExp>
     `(lambda ,(unrewrite-arglist exp)
	,(unrewrite (|@| body exp))))
    (<gnu.expr.ReferenceExp> (! get-symbol exp))
    (<gnu.expr.ApplyExp> (unrewrite-apply exp))
    (<gnu.expr.BeginExp> `(begin . ,(unrewrite* (! get-expressions exp))))
    (<gnu.expr.IfExp> 
     `(if ,(unrewrite (! get-test exp))
	  ,(unrewrite (! get-then-clause exp))
	  ,@(let ((eclause (! get-else-clause exp)))
	      (if (eq? eclause #!null) '()
		  (list (unrewrite eclause))))))
    (#t exp)))

(define (unrewrite-arglist (exp <gnu.expr.LambdaExp>))
  (let* ((min (|@| min_args exp))
	 (rest? (negative? (|@| max_args exp)))
	 (key? (not (eq? (|@| keywords exp) #!null)))
	 (opt (|@| opt_args exp))
	 (required '())
	 (optional '())
	 (key '())
	 (rest #f)
	 (aux '()))
    (do ((decl :: <gnu.expr.Declaration> (! first-decl exp)
	       (! next-decl decl))
	 (i 0 (+ i 1)))
	((eq? decl #!null))
      (let ((var (! get-symbol decl)))
	(cond ((< i min) 
	       (set! required (cons var required)))
	      ((< i (+ min opt))
	       (set! optional (cons var optional)))
	      ((and rest? (= i (+ min opt)))
	       (set! rest var))
	      ((and key? (< i (+ min opt 
				 (if rest? 1 0)
				 (|@| length (|@| keywords exp)))))
	       (set! key (cons var key)))
	      (#t 
	       (error "nyi")))))
    `(,@(reverse required)
      ,@(cond ((zero? opt) ())
	      (#t `(#!optional . ,(reverse optional))))
      ,@(cond (rest? `(#!rest ,rest))
	      (#t '()))
      ,@(cond (key? `(#!key . ,(reverse key)))
	      (#t '())))))

(define (unrewrite* (exps <gnu.expr.Expression[]>))
  (packing (pack)
    (do ((len (|@| length exps))
	 (i 0 (+ i 1)))
	((= i len))
      (pack (unrewrite (exps i))))))

(define (unrewrite-let (exp <gnu.expr.LetExp>))
  `(let ,(packing (pack)
           (do ((decl :: <gnu.expr.Declaration> 
		      (! first-decl exp) (! next-decl decl))
                (i 0 (+ i 1)))
               ((eq? decl #!null))
             (pack (list (! get-symbol decl)
                         (unrewrite (! getInitValue decl))))))
     ,(unrewrite (|@| body exp))))

(define (unrewrite-quote (exp <gnu.expr.QuoteExp>))
  (let ((val (|@| value exp))
        (type-name (lambda (name) (string->symbol (format "<~a>" name)))))
    (typecase val
      ((or <number> <boolean> <character> <keyword> <string> 
           (eql #!undefined) (eql #!eof))
       val)
      (<gnu.bytecode.Type> (type-name (! get-name val)))
      (<java.lang.Class> (type-name (! get-name val)))
      (#t `(quote ,val)))))

(define (unrewrite-apply (exp <gnu.expr.ApplyExp>))
  (let* ((fun (! get-function exp))
         (args (unrewrite* (! get-args exp)))
         (fbinding (typecase fun
                     (<gnu.expr.ReferenceExp> (! get-binding fun))
                     (#t #!null)))
         (apply-to-args (<gnu.expr.Declaration>:getDeclarationFromStatic
                         "kawa.standard.Scheme" "applyToArgs"))
         (fval (! get-function-value exp)))
    (cond ((and (not (eq? fbinding #!null))
		(not (eq? apply-to-args #!null))
                (eq? (|@| field fbinding)
		     (|@| field apply-to-args)))
           args)
          ((typecase fval
             (<gnu.kawa.functions.Convert>
              `(as . ,args))
             (<gnu.kawa.functions.GetNamedPart>
              `(\: . ,args))
             (#t #f)))
          (#t
           `(,(unrewrite fun) . ,args)))))
