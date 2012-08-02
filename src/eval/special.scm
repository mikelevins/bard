;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $special-forms-table (make-table test: eq?))

(define (%defspecial nm eval-fn)
  (table-set! $special-forms-table nm eval-fn))

(define (%special-evaluator nm)
  (table-ref $special-forms-table nm #f))

(define (%special-form? expr)
  (and (table-ref $special-forms-table (%car expr) #f)
       #t))

(define (%eval-special-form expr env)
  (let* ((op (%car expr))
         (evaluator (table-ref $special-forms-table op #f)))
    (if evaluator
        (evaluator expr env)
        (error (string-append "unrecognized special form" (%as-string (%car expr)))))))


;;; ----------------------------------------------------------------------
;;; special forms defined
;;; ----------------------------------------------------------------------

;;; and
;;; ----------------------------------------------------------------------

;;; begin
;;; ----------------------------------------------------------------------

(%defspecial 'begin
             (lambda (expr env) 
               (%eval-sequence (%cdr expr) env)))

;;; cond
;;; ----------------------------------------------------------------------

(%defspecial 'cond
             (lambda (expr env)
               (let loop ((clauses (%cdr expr)))
                 (if (%null? clauses)
                     (%nothing)
                     (let* ((clause (%car clauses))
                            (test (%car clause))
                            (conseq (%cdr clause)))
                       (if (eq? 'else test)
                           (%eval-sequence conseq env)
                           (if (%true? (%eval test env))
                               (%eval-sequence conseq env)
                               (loop (%cdr clauses)))))))))

;;; define
;;; ----------------------------------------------------------------------

(%defspecial 'define
             (lambda (expr env)
               (%defglobal (%list-ref expr 1) (%eval (%list-ref expr 2) env))
               (%list-ref expr 1)))

;;; define-schema
;;; ----------------------------------------------------------------------

(define (%canonicalize-slot-spec spec)
  (let ((spec (if (symbol? spec)
                  (list spec default: (%nothing))
                  (if (list? spec)
                      (list (car spec)
                            default:
                            (getf default: spec (%nothing)))
                      (error (string-append "Invalid slot spec: "
                                            (object->string spec)))))))
    spec))

(define (%parse-canonical-slot-description spec env)
  (cons (car spec)
        (%eval (getf default: spec (%nothing)) env)))

(define (%parse-slot-descriptions specs env)
  (let ((specs (map %canonicalize-slot-spec specs)))
    (map (lambda (s) (%parse-canonical-slot-description s env))
         specs)))

(%defspecial 'define-schema
             (lambda (expr env)
               (let* ((sname (list-ref expr 1))
                      (includes (map (lambda (e)(%eval e env))
                                     (list-ref expr 2)))
                      (slot-specs (drop 3 expr))
                      (slots (%parse-slot-descriptions slot-specs env))
                      (sc (%make-schema sname includes slots)))
                 (table-set! $bard-global-variables sname sc)
                 sc)))

;;; define-macro prototype & body
;;; ----------------------------------------------------------------------

(%defspecial 'define-macro
             (lambda (expr env)
               (let* ((proto (cadr expr))
                      (body (cddr expr))
                      (mname (car proto))
                      (marglist (cdr proto))
                      (expander (%eval `(method ,marglist (begin ,@body)) env)))
                 (%define-macro-function mname
                                         (lambda (expr)
                                           (%apply expander (%cdr expr)))))))

;;; define-function
;;; ----------------------------------------------------------------------

;;; (show (%eval (bard:read-from-string "(define-function (always-true) true)")))
;;; (show (%eval (bard:read-from-string "(define-function (itself x) x)")))
;;; (show (%eval (bard:read-from-string "(define-function (my-list & args) args)")))
;;; (show (%eval (bard:read-from-string "(define-function (foo {a: 1 b: 2}) (list a b))")))
;;; (show (%eval (bard:read-from-string "(define-function (elt (ls <list>)(i <fixnum>)) (%list-ref ls i))")))
;;; (show (%eval (bard:read-from-string "(define-function (frob (x <list>)(y <fixnum>) & more-args) (%list-ref ls i))")))

(define %fparams %list)

(define (%parse-function-parameters params env)
  (let loop ((params params)
             (param-names %nil)
             (param-types %nil)
             (rest-arg #f)
             (frame-arg #f))
    (if (%null? params)
        (%fparams param-names param-types rest-arg frame-arg)
        (let ((next (%car params))
              (more (%cdr params)))
          (cond
           ((eq? '& next) (loop %nil
                                param-names
                                param-types
                                (%cadr params)
                                frame-arg))
           ((symbol? next) (loop (%cdr params)
                                 (%append param-names (%list next))
                                 (%append param-types (%list 'Anything))
                                 rest-arg
                                 frame-arg))
           ((%list? next) (if (eq? 'frame (%car next))
                              (loop %nil
                                    param-names
                                    param-types
                                    rest-arg
                                    next)
                              (loop (%cdr params)
                                    (%append param-names (%list (%car next)))
                                    (%append param-types (%list (%cadr next)))
                                    rest-arg
                                    frame-arg)))
           (else (error (string-append "Invalid parameter: " (object->string next)))))))))

(define (%parse-function-prototype proto env)
  (let* ((name (%car proto))
         (params (%cdr proto)))
    (%cons name (%parse-function-parameters params env))))

(define (%fproto-name fp)(%list-ref fp 0))
(define (%fproto-formals fp)(%list-ref fp 1))
(define (%fproto-types fp)(%list-ref fp 2))
(define (%fproto-restarg fp)(%list-ref fp 3))
(define (%fproto-framearg fp)(%list-ref fp 4))


;;; TODO: define-function can now parse frame args, e.g.:
;;; (define-function (x y {a: "default1" b: "default2"})...)
;;; but functions do not yet handle them
(define (%define-function expr #!optional (env (%null-environment)))
  (let* ((prototype (%parse-function-prototype (%list-ref expr 1) env))
         (fname (%fproto-name prototype))
         (fn (or (table-ref $bard-global-variables fname #f)
                 (let ((f (%make-function name: fname)))
                   (%defglobal fname f)
                   f)))
         (formals (%fproto-formals prototype))
         (types (%map (lambda (p)(%eval p env))
                      (%fproto-types prototype)))
         (restarg (%fproto-restarg prototype))
         (framearg (%fproto-framearg prototype))
         (required-count (%length types))
         (body (%cons 'begin (%drop 2 expr)))
         (method-signature types)
         (method (%make-interpreted-method formals body
                                           environment: env
                                           name: fname
                                           required-count: required-count
                                           restarg: restarg)))
    (%add-method! fn method-signature method)
    fname))

(%defspecial 'define-function %define-function)

;;; function
;;; ----------------------------------------------------------------------

(%defspecial 'function
             (lambda (expr env)
               (if (> (%length expr) 1)
                   (%make-function name: (%list-ref expr 1))
                   (%make-function))))

;;; generate
;;; ----------------------------------------------------------------------
;;; (generate ((x 1))
;;;           (yield (* x x))
;;;           (then (+ x 1)))

(%defspecial 'generate
             (lambda (expr env)
               (let* ((env (%copy-environment env))
                      (bindings (map (lambda (binding)
                                       (%list (%car binding)
                                              (%eval (%cadr binding) env)))
                                     (%list-ref expr 1)))
                      (yield-expr (let ((xp (%list-ref expr 2)))
                                    (if (eq? 'yield (car xp))
                                        (cdr xp)
                                        (error (string-append "Invalid yield form in generate: "
                                                              (object->string xp))))))
                      (then-expr (let ((xp (%list-ref expr 3)))
                                   (if (eq? 'then (car xp))
                                       (cdr xp)
                                       (error (string-append "Invalid then form in generate: "
                                                             (object->string xp)))))))
                 (%make-generator yield-expr then-expr env bindings))))

;;; if
;;; ----------------------------------------------------------------------

(%defspecial 'if
             (lambda (expr env)
               (let ((test (%list-ref expr 1))
                     (conseq (%list-ref expr 2))
                     (alt? (> (%length expr) 3)))
                 (if (%true? (%eval test env))
                     (%eval conseq env)
                     (if alt?
                         (%eval (%list-ref expr 3) env)
                         (%nothing))))))

;;; let
;;; ----------------------------------------------------------------------

(%defspecial 'let 
             (lambda (expr env)
               (let ((bindings (%list-ref expr 1))
                     (body (%drop 2 expr)))
                 (%eval-sequence body (%add-let-bindings env bindings)))))


;;; method
;;; ----------------------------------------------------------------------

;;; method-form => (method-name formals restarg body)
;;; (method (x y) (+ x y)) => (#f (x y) #f (begin (+ x y)))
;;; (method foo (x y) (+ x y)) => (foo (x y) #f (begin (+ x y)))
;;; (method foo (x y) (begin (+ x y))) => (foo (x y) #f (begin (+ x y)))
;;; (method foo (x y) (+ x y)(* x y)) => (foo (x y) #f (begin (+ x y)(* x y)))
;;; (method (x y & more) (+ x y)) => (#f (x y) more (begin (+ x y)))

(define (%parse-method-parameters params)
  (let loop ((ps params)
             (formals %nil))
    (if (%null? ps)
        (values formals #f)
        (if (eq? '& (%car ps))
            (let* ((ps (%cdr ps))
                   (len (%length ps)))
              (case len
               ((0)(error "An ampersand must be followed by a parameter name"))
               ((1)(values formals (%car ps)))
               (else (error "Too many parameters following an ampersand"))))
            (loop (%cdr ps)(%append formals (%list (%car ps))))))))

(define (%parse-method-form m)
  (let* ((form (%cdr m))
         (first (%car form))
         (mname (if (symbol? first) first #f))
         (params (if mname (%list-ref form 1)(%list-ref form 0)))
         (body (%cons 'begin (if mname (%drop 2 form)(%drop 1 form)))))
    (receive (formals restarg)(%parse-method-parameters params)
             (%list mname formals restarg body))))

(define (%mdesc-get-name mdesc)(%list-ref mdesc 0))
(define (%mdesc-get-formals mdesc)(%list-ref mdesc 1))
(define (%mdesc-get-restarg mdesc)(%list-ref mdesc 2))
(define (%mdesc-get-body mdesc)(%list-ref mdesc 3))

(%defspecial 'method
             (lambda (expr env)
               (let* ((mdesc (%parse-method-form expr))
                      (mname (%mdesc-get-name mdesc))
                      (formals (%mdesc-get-formals mdesc))
                      (restarg (%mdesc-get-restarg mdesc))
                      (body (%mdesc-get-body mdesc)))
                 (%make-interpreted-method formals body 
                                           environment: env
                                           name: mname
                                           required-count: (%length formals)
                                           restarg: restarg))))

;;; not
;;; ----------------------------------------------------------------------

(%defspecial 'not
             (lambda (expr env)
               (if (%true? (%eval (%car (%cdr expr)) env))
                   (%false)
                   (%true))))


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
	    (%cadr exp)
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
	    (list 'append (%cadr (%car exp))
		  (%expand-quasiquote (cdr exp) nesting))
	    (combine-skeletons (%expand-quasiquote (car exp) (- nesting 1))
			       (%expand-quasiquote (cdr exp) nesting)
			       exp)))
       (else (combine-skeletons (%expand-quasiquote (car exp) nesting)
				(%expand-quasiquote (cdr exp) nesting)
				exp))))


(%defspecial 'quasiquote 
             (lambda (expr env)
               (%eval (%expand-quasiquote (%cadr expr) 0) env)))

(%defspecial 'unquote (lambda (expr env) (error "invalid context for unquote")))
(%defspecial 'unquote-splicing (lambda (expr env) (error "invalid context for unquote-splicing")))

;;; quote
;;; ----------------------------------------------------------------------

(%defspecial 'quote 
             (lambda (expr env)
               (if (= 2 (%length expr))
                   (%car (%cdr expr))
                   (error (string-append "Wrong number of arguments to quote: " (%as-string (%cdr expr)))))))

;;; repeat
;;; ----------------------------------------------------------------------

(%defspecial 'repeat
             (lambda (expr env)
               (let ((form (%cdr expr)))
                 (let loop ()
                   (%eval-sequence form env)
                   (loop)))))

;;; set!
;;; ----------------------------------------------------------------------

(%defspecial 'set!
             (lambda (expr env)
               (%set-variable! (%list-ref expr 1)
                               (%eval (%list-ref expr 2)
                                      env)
                               env)))

;;; time
;;; ----------------------------------------------------------------------

(%defspecial 'time
             (lambda (expr env)
               (time (%eval (%car (%cdr expr)) env))))


;;; with-open-file
;;; ----------------------------------------------------------------------
;;; (with-open-file (var path {direction: 'input}) (do-stuff-to-file-stream var))

(%defspecial 'with-open-file
             (lambda (expr env)
               (let* ((spec (%list-ref expr 1))
                      (var (%car spec))
                      (path (%eval (%cadr spec) env))
                      (keyargs (%drop 2 spec))
                      (direction (let ((keylen (%length keyargs)))
                                   (if (<= keylen 0)
                                       'input
                                       (if (and (= 2 keylen)
                                                (eq? direction:))
                                           (%eval (%cadr keyargs) env)
                                           (error (string-append "Invalid keyword arguments to with-open-file: "
                                                                 (%as-string keyargs)))))))
                      (body (%cons 'begin (%drop 2 expr))))
                 (case direction
                   ((input in) (call-with-input-file path
                                 (lambda (in)
                                   (let ((env (%add-binding env var in)))
                                     (%eval body env)))))
                   ((output out) (call-with-output-file path
                                   (lambda (out)
                                     (let ((env (%add-binding env var out)))
                                       (%eval body env)))))
                   (else (error (string-append "Invalid value for direction: argument: "
                                               (object->string direction))))))))


