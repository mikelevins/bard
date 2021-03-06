;;;; ***********************************************************************
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

(define $special-forms-table (make-table test: eq?))

(define (%defspecial nm eval-fn)
  (table-set! $special-forms-table nm eval-fn))

(define (%special-evaluator nm)
  (table-ref $special-forms-table nm #f))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (car expr) #f)
       #t))

(define (%eval-special-form expr env)
  (let* ((op (car expr))
         (evaluator (table-ref $special-forms-table op #f)))
    (if evaluator
        (evaluator expr env)
        (error (str "unrecognized special form: " (car expr))))))

;;; ----------------------------------------------------------------------
;;; special forms defined
;;; ----------------------------------------------------------------------

;;; add-method!
;;; ----------------------------------------------------------------------

(%defspecial 'add-method!
             (lambda (expr env) 
               (let* ((fn-ref (list-ref expr 1))
                      (fn (%eval fn-ref env))
                      (types-expr (list-ref expr 2))
                      (types (map (lambda (t)(%eval t env)) types-expr))
                      (method-ref (list-ref expr 3))
                      (method (%eval method-ref env))
                      (method-signature (make-signature types #f `(,Anything))))
                 (if (signature-congruent? method-signature
                                           (car (function-signatures fn)))
                     (%add-method! fn types method)
                     (error (str "Conflicting input types: expected one of "
                                 (str (map %as-string (function-signatures fn)))
                                 " but found "
                                 (%as-string types))))
                 fn)))

;;; begin
;;; ----------------------------------------------------------------------

(%defspecial 'begin
             (lambda (expr env) 
               (%eval-sequence (cdr expr) env)))


;;; bind
;;; ----------------------------------------------------------------------

(%defspecial 'bind
             (lambda (expr env)
               (let ((bindings (list-ref expr 1))
                     (body (drop 2 expr)))
                 (%eval-sequence body (%add-let-bindings env bindings)))))

;;; cond
;;; ----------------------------------------------------------------------

(%defspecial 'cond
             (lambda (expr env)
               (let loop ((clauses (cdr expr)))
                 (if (null? clauses)
                     '()
                     (let* ((clause (car clauses))
                            (test (car clause))
                            (conseq (cdr clause)))
                       (if (eq? 'else test)
                           (%eval-sequence conseq env)
                           (if (%true? (%eval test env))
                               (%eval-sequence conseq env)
                               (loop (cdr clauses)))))))))

;;; def
;;; ----------------------------------------------------------------------

(define (%eval-def expr env)
  (%defglobal (list-ref expr 1) (%eval (list-ref expr 2) env)))

(%defspecial 'def (lambda (expr env) (%eval-def expr env)))

;;; define
;;; ----------------------------------------------------------------------

;;; variable

(define (%eval-define-variable expr env)
  (%defglobal (list-ref expr 2) (%eval (list-ref expr 3) env))
  (list-ref expr 2))

;;; macro

(define (%eval-define-macro expr env)
  (let* ((proto (caddr expr))
         (body (cdddr expr))
         (mname (car proto))
         (marglist (cdr proto))
         (expander (%eval `(method ,marglist (begin ,@body)) env)))
    (%define-macro-function mname
                            (lambda (expr)
                              (%apply expander (cdr expr))))))

;;; method

(define (%parse-method-prototype prototype env)
  (let ((pos (position (lambda (x)(eq? x '&)) prototype)))
    (if pos
        ;; there's a '&' so we have a restarg
        (values (car prototype)
                (drop 1 (take pos prototype))
                (let ((restarg (car (drop (+ 1 pos) prototype))))
                  (if (symbol? restarg)
                      ;; the restarg is a symbol; we'll bind it to a
                      ;; list of the remaining args
                      restarg
                      ;; the restarg is a table expression; we'll use
                      ;; it to match keyword arguments
                      (%eval restarg env))))
        ;; there's no '&' so we don't have a restarg
        (values (car prototype)
                (drop 1 prototype)
                #f))))

;;; processing type constraints
;;; 
;;; (define method (foo x y z)
;;;   where: {y: <fixnum>)
;;;           z: (equals 1))}
;;;   ...)
;;;
;;; TODO: not yet implemented; still processes old syntax from bard 0.3
;;; UPDATE: with: is now changed to where:
;;;
;;; to make this work, here's what I need to do:
;;; TODO:
;;; - change the reader to turn {...} into a syntax object that
;;;   I can process appropriately, considering whether it's in the
;;;   where: clause of a define method
;;; - convert keywords in the {} to lexical variable names
;;; - evaluate the values for the keys to use as matching tests
;;; - implement apply for types to mean:
;;;   (<type> thing) -> Boolean (true if thing is an instance of <type>)
;;; - implement instance? for classes to mean true if Bard can find
;;;   a function that specializes an input class on the value's structure

(define (%parse-type-constraints formals constraints env)
  (let loop ((params formals)
             (types '()))
    (if (null? params)
        (begin
          (reverse types))
        (let* ((param (car params))
               (constraint (assq param constraints))
               (type (if constraint
                         (if (symbol? (cadr constraint))
                             (%eval (cadr constraint) env)
                             (if (and (pair? (cadr constraint))
                                      (eq? 'exactly (car (cadr constraint))))
                                 (%singleton (cadr (cadr constraint)))
                                 (error (str "Invalid type constraint: " constraint))))
                         Anything)))
          (loop (cdr params)
                (cons type types))))))

(define (%eval-define-method expr #!optional (env (%null-environment)))
  (let* ((prototype (list-ref expr 2))
         (with-arg? (let ((tl (drop 3 expr)))
                      (if (null? tl)
                          #f
                          (if (eq? where: (car tl))
                              (cadr tl)
                              #f))))
         (constraints (if with-arg?
                          (list-ref expr 4)
                          '()))
         (body (cons 'begin
                     (if with-arg?
                         (drop 5 expr)
                         (drop 3 expr)))))
    (receive (fname formals restarg)
             (%parse-method-prototype prototype env)
             (let* ((input-types (%parse-type-constraints formals constraints env))
                    (output-types `(,Anything))
                    (fn (table-ref $bard-global-variables fname #f))
                    (sig (make-signature input-types restarg output-types)))
               (cond
                ((not fn)
                 (let ((fn (make-function debug-name: fname
                                          signatures: (list sig)))
                       (method (make-interpreted-method formal-parameters: formals
                                                        body: body
                                                        environment: env
                                                        debug-name: fname
                                                        restarg: restarg)))
                   (%defglobal fname fn)
                   (%add-method! fn input-types method)
                   fname))
                ((signature-congruent? sig (car (function-instance-signatures fn)))
                 (let ((method (make-interpreted-method formal-parameters: formals
                                                        body: body
                                                        environment: env
                                                        debug-name: fname
                                                        restarg: restarg)))
                   (%add-method! fn input-types method)
                   fname))
                (else: (error (str "Conflicting function definition for " fname
                                   "; you must undefine the function before you can add this definition"))))))))

;;; class

(define (%eval-define-class expr env)
  (let* ((cname (list-ref expr 2))
         (class (%make-class cname)))
    (%defglobal cname class)
    class))

;;; protocol

;;; (define protocol Rational
;;;   (numerator Ratio -> Integer)
;;;   (denominator Ratio -> Integer))

;;; when defining a protocol, we define a global variable with the
;;; protocol's name and bind the protocol object to it.  we also
;;; create a function corresponding to each function specified in the
;;; define protocol expression, and add each function to the new
;;; protocol. Finally, we also define a global variable for each
;;; function name and bind it to the corresponding function.

;;; a couple of things can complicate matters:

;;; if the protocol already exists, then we don't create a new
;;; one. Instead, we attempt to add functions to the existing
;;; protocol.

;;; if a function of the same name already exists, we check the
;;; signatures. If they differ, we don't replace the existing one;
;;; instead, we print a warning that it was skipped and move on. In
;;; that case the programmer must undefine the existing function
;;; before executing the new definition.

;;; If the signatures are the same, we silently skip the function
;;; definition, and we add it to the protocol only if it's not already
;;; there, because functions with equal signatures are for all
;;; practical purposes identical.

(define (%make-named-protocol-function nm spec env)
  (cons nm (%eval (cons 'function spec) env)))

(define (%maybe-make-named-protocol-function nm spec env)
  (if (%globally-bound? nm)
      (let ((found-val (%global-value nm)))
        (if (function? found-val)
            (if (signature-congruent? spec (car (function-instance-signatures found-val)))
                (cons nm found-val)
                (begin
                  (warn (str "the function " nm " is defined with a different signature: "
                             (map signature->string (function-instance-signatures found-val))
                             "; definition not changed"))
                  #f))
            (begin
              (warn (str nm " is defined, and is not a function; definition not changed"))
              #f)))
      (%make-named-protocol-function nm spec env)))

(define (%build-protocol-functions-alist protocol function-specs env)
  (let* ((fnames (map car function-specs))
         (fspecs (map cdr function-specs))
         (named-fns (map (lambda (nm spec)(%maybe-make-named-protocol-function nm spec env))
                         fnames fspecs)))
    (remv #f named-fns)))

(define (%maybe-add-protocol-function! protocol fname/fn)
  (let* ((fname (car fname/fn))
        (fn (cdr fname/fn))
        (already (table-ref (protocol-instance-functions protocol) fname #f)))
    (if already
        (if (signature-congruent? (car (function-signatures fn))
                                  (car (function-signatures already)))
            ;; found the function in the protocol
            ;; the new signature fits; add it to the found function
            (begin
              (function-add-signatures! already (function-signatures fn))
              already)
            ;; the new signature conflicts; warn and bail out
            (begin
              (warn (str fname " is defined on protocol" (protocol-instance-name protocol) 
                         " with an incompatible signature; definition not changed"))
              #f))
        (begin
          (%protocol-add! protocol fname fn)
          fn))))

(define (%eval-define-protocol expr env) 
  (let* ((pname (list-ref expr 2))
         (function-specs (drop 3 expr))
         (protocol (if (%globally-bound? pname)
                       (%eval pname env)
                       (let ((proto (%make-protocol pname)))
                         (register-protocol pname proto)
                         proto)))
         (functions-alist (%build-protocol-functions-alist protocol function-specs env)))
    (for-each (lambda (fname/fn)(%maybe-add-protocol-function! protocol fname/fn))
              functions-alist)
    (if (not (and (%globally-bound? pname)
                  (eq? protocol (%global-value pname))))
        (%defglobal pname protocol))
    protocol))

;;; define

(%defspecial 'define
             (lambda (expr env)
               (let ((kind (list-ref expr 1)))
                 (cond
                  ((eq? 'class kind)(%eval-define-class expr env))
                  ((eq? 'macro kind)(%eval-define-macro expr env))
                  ((eq? 'method kind)(%eval-define-method expr env))
                  ((eq? 'protocol kind)(%eval-define-protocol expr env))
                  ((eq? 'variable kind)(%eval-define-variable expr env))
                  (else (error (str "Unrecognized definition type: " kind)))))))


;;; ensure
;;; ----------------------------------------------------------------------

(%defspecial 'ensure
             (lambda (expr env)
               (let* ((before `(,@(list-ref expr 1)))
                      (during `(,@(list-ref expr 2)))
                      (after `(,@(list-ref expr 3))))
                 (dynamic-wind 
                     (lambda ()(%eval before env))
                     (lambda ()(%eval during env))
                     (lambda ()(%eval after env))))))

;;; function
;;; ----------------------------------------------------------------------

(%defspecial 'function
             (lambda (expr env)
               (if (> (length expr) 1)
                   (let ((arrow-pos (position (lambda (x)(eq? '-> x)) expr))
                         (ampersand-pos (position (lambda (x)(eq? '& x)) expr)))
                     (if arrow-pos
                         (let* ((in-types (if ampersand-pos
                                              (drop 1 (take ampersand-pos expr))
                                              (drop 1 (take arrow-pos expr))))
                                (restarg (if ampersand-pos
                                             (list-ref expr (+ 1 ampersand-pos))
                                             #f))
                                (out-types (drop (+ 1 arrow-pos) expr))
                                (sig (make-signature in-types restarg out-types)))
                           (make-function debug-name: #f
                                          signatures: (list sig)))
                         (error (str "Invalid function syntax: " expr))))
                   (error (str "Invalid function syntax: " expr)))))

;;; generate
;;; ----------------------------------------------------------------------

(%defspecial 'generate 
             (lambda (expr env)
               (let* ((bindings (list-ref expr 1))
                      (vars (map car bindings))
                      (initvals (%eval `(let ,bindings (list ,@vars)) env))
                      (body (drop 2 expr)))
                 (make-generator vars initvals body env))))


;;; if
;;; ----------------------------------------------------------------------

(%defspecial 'if
             (lambda (expr env)
               (let ((test (list-ref expr 1))
                     (conseq (list-ref expr 2))
                     (alt? (> (length expr) 3)))
                 (if (%true? (%eval test env))
                     (%eval conseq env)
                     (if alt?
                         (%eval (list-ref expr 3) env)
                         '())))))


;;; loop
;;; ----------------------------------------------------------------------

(define (%loop-name expr)(list-ref expr 1))
(define (%loop-bindings expr)(list-ref expr 2))
(define (%loop-vars bindings)(map car bindings))
(define (%loop-vals bindings)(map cadr bindings))
(define (%loop-body expr)(drop 3 expr))

(%defspecial 'loop
             (lambda (expr env)
               (let* ((loopname (%loop-name expr))
                      (bindings (%loop-bindings expr))
                      (loopvars (%loop-vars bindings))
                      (loopvals (%loop-vals bindings))
                      (loopbody (%loop-body expr))
                      (loopenv (%add-binding env loopname #f))
                      (loopmethod (make-interpreted-method formal-parameters: loopvars
                                                           body: (cons 'begin loopbody) 
                                                           environment: env
                                                           debug-name: loopname
                                                           restarg: #f)))
                 (set-interpreted-method-environment! loopmethod
                                                      (%add-binding
                                                       (interpreted-method-environment loopmethod)
                                                       loopname loopmethod))
                 (%eval `(,loopmethod ,@loopvals) loopenv))))

;;; match
;;; ----------------------------------------------------------------------

(%defspecial 'match (lambda (expr env) (error "match not yet implemented")))


;;; method
;;; ----------------------------------------------------------------------

(define (%parse-method-parameters params)
  (let loop ((ps params)
             (formals '()))
    (if (null? ps)
        (values formals #f)
        (if (eq? '& (car ps))
            (let* ((ps (cdr ps))
                   (len (length ps)))
              (case len
               ((0)(error "An ampersand must be followed by a parameter name"))
               ((1)(values formals (car ps)))
               (else (error "Too many parameters following an ampersand"))))
            (loop (cdr ps)(append formals (list (car ps))))))))

(define (%parse-method-form m)
  (let* ((form (cdr m))
         (first (car form))
         (mname (if (symbol? first) first #f))
         (params (if mname (list-ref form 1)(list-ref form 0)))
         (body (cons 'begin (if mname (drop 2 form)(drop 1 form)))))
    (receive (formals restarg)(%parse-method-parameters params)
             (list mname formals restarg body))))

(define (%mdesc-get-name mdesc)(list-ref mdesc 0))
(define (%mdesc-get-formals mdesc)(list-ref mdesc 1))
(define (%mdesc-get-restarg mdesc)(list-ref mdesc 2))
(define (%mdesc-get-body mdesc)(list-ref mdesc 3))

(define %eval-method-form
  (lambda (expr env)
    (let* ((mdesc (%parse-method-form expr))
           (mname (%mdesc-get-name mdesc))
           (formals (%mdesc-get-formals mdesc))
           (restarg (%mdesc-get-restarg mdesc))
           (body (%mdesc-get-body mdesc)))
      (make-interpreted-method formal-parameters: formals
                               body: body 
                               environment: env
                               debug-name: mname
                               restarg: restarg))))

(%defspecial '^ %eval-method-form)
(%defspecial 'method %eval-method-form)

;;; not
;;; ----------------------------------------------------------------------

(%defspecial 'not
             (lambda (expr env)
               (if (%true? (%eval (car (cdr expr)) env))
                   #f
                   #t)))

;;; protocol
;;; ----------------------------------------------------------------------

(define (%eval-make-protocol expr env) 
  (let* ((pname (list-ref expr 1))
         (function-specs (drop 2 expr))
         (protocol (%make-protocol pname))
         (functions-alist (%build-protocol-functions-alist protocol function-specs env)))
    (for-each (lambda (fname/fn)(%maybe-add-protocol-function! protocol fname/fn))
              functions-alist)
    protocol))

(%defspecial 'protocol %eval-make-protocol)

;;; quasiquote
;;; ----------------------------------------------------------------------
;;; after norvig

(define (constant? exp)
  (if (pair? exp)
      (eq? (car exp) 'quote)
      (not (symbol? exp))))

(define (combine-skeletons left right exp)
  (cond
   ((and (constant? left) (constant? right)) 
    (if (and (eqv? (%eval left) (car exp))
             (eqv? (%eval right) (cdr exp)))
        (list 'quote exp)
        (list 'quote (cons (%eval left) (%eval right)))))
   ((null? right) (list 'list left))
   ((and (pair? right) (eq? (car right) 'list))
    (cons 'list (cons left (cdr right))))
   (else (list 'add-first left right))))

(define (%expand-quasiquote exp nesting)
  (cond
   ((not (pair? exp)) 
    (if (constant? exp) exp (list 'quote exp)))
   ((and (eq? (car exp) 'unquote) (= (length exp) 2))
    (if (= nesting 0)
        (cadr exp)
        (combine-skeletons ''unquote 
                           (%expand-quasiquote (cdr exp) (- nesting 1))
                           exp)))
   ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
    (combine-skeletons ''quasiquote 
                       (%expand-quasiquote (cdr exp) (+ nesting 1))
                       exp))
   ((and (pair? (car exp))
         (eq? (caar exp) 'unquote-splicing)
         (= (length (car exp)) 2))
    (if (= nesting 0)
        (list 'append (cadr (car exp))
              (%expand-quasiquote (cdr exp) nesting))
        (combine-skeletons (%expand-quasiquote (car exp) (- nesting 1))
                           (%expand-quasiquote (cdr exp) nesting)
                           exp)))
   (else (combine-skeletons (%expand-quasiquote (car exp) nesting)
                            (%expand-quasiquote (cdr exp) nesting)
                            exp))))


(%defspecial 'quasiquote 
             (lambda (expr env)
               (%eval (%expand-quasiquote (cadr expr) 0) env)))

(%defspecial 'unquote (lambda (expr env) (error "invalid context for unquote")))
(%defspecial 'unquote-splicing (lambda (expr env) (error "invalid context for unquote-splicing")))

;;; quote
;;; ----------------------------------------------------------------------

(%defspecial 'quote 
             (lambda (expr env)
               (if (= 2 (length expr))
                   (car (cdr expr))
                   (error (str "Wrong number of arguments to quote: " (cdr expr))))))

;;; receive
;;; ----------------------------------------------------------------------

(%defspecial 'receive (lambda (expr env) (error "receive not yet implemented")))


;;; remove-method!
;;; ----------------------------------------------------------------------

(%defspecial 'remove-method!
             (lambda (expr env) 
               (let* ((fn-ref (list-ref expr 1))
                      (fn (%eval fn-ref env))
                      (types-expr (list-ref expr 2))
                      (types (map (lambda (t)(%eval t env)) types-expr)))
                 (%remove-method! fn types))))

;;; repeat
;;; ----------------------------------------------------------------------

(%defspecial 'repeat
             (lambda (expr env)
               (let ((form (cdr expr)))
                 (let loop ()
                   (%eval-sequence form env)
                   (loop)))))

;;; send
;;; ----------------------------------------------------------------------

(%defspecial 'send (lambda (expr env)(error "send not yet implemented")))

;;; set!
;;; ----------------------------------------------------------------------

(%defspecial 'set!
             (lambda (expr env)
               (%set-variable! (list-ref expr 1)
                               (%eval (list-ref expr 2)
                                      env)
                               env)))

;;; time
;;; ----------------------------------------------------------------------

(%defspecial 'time
             (lambda (expr env)
               (time (%eval (car (cdr expr)) env))))

;;; undefine
;;; ----------------------------------------------------------------------

(%defspecial 'undefine
             (lambda (expr env)
               (%remglobal (list-ref expr 1))))

;;; unless
;;; ----------------------------------------------------------------------

(%defspecial 'unless
             (lambda (expr env)
               (let ((test (list-ref expr 1))
                     (body (cons 'begin (drop 2 expr))))
                 (if (%false? (%eval test env))
                     (%eval body env)
                     '()))))

;;; values
;;; ----------------------------------------------------------------------

(%defspecial 'values
             (lambda (expr env)
               (apply values (map (lambda (x)(%eval x env))
                                  (cdr expr)))))

;;; when
;;; ----------------------------------------------------------------------

(%defspecial 'when
             (lambda (expr env)
               (let ((test (list-ref expr 1))
                     (body (cons 'begin (drop 2 expr))))
                 (if (%true? (%eval test env))
                     (%eval body env)
                     '()))))


;;; with-open-file
;;; ----------------------------------------------------------------------
;;; (with-open-file (var path {direction: 'input}) (do-stuff-to-file-stream var))

(%defspecial 'with-open-file
             (lambda (expr env)
               (let* ((spec (list-ref expr 1))
                      (var (car spec))
                      (path (%eval (cadr spec) env))
                      (keyargs (drop 2 spec))
                      (direction (let ((keylen (length keyargs)))
                                   (if (<= keylen 0)
                                       'input
                                       (if (and (= 2 keylen)
                                                (eq? direction: (car keyargs)))
                                           (%eval (cadr keyargs) env)
                                           (error (str "Invalid keyword arguments to with-open-file: "
                                                       keyargs))))))
                      (body (cons 'begin (drop 2 expr))))
                 (case direction
                   ((input in) (call-with-input-file path
                                 (lambda (in)
                                   (let ((env (%add-binding env var in)))
                                     (%eval body env)))))
                   ((output out) (call-with-output-file path
                                   (lambda (out)
                                     (let ((env (%add-binding env var out)))
                                       (%eval body env)))))
                   (else (error (str "Invalid value for direction: argument: "
                                     direction)))))))


