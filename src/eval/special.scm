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

(%defspecial 'and 
             (lambda (expr env)
               (let loop ((expr (%cdr expr))
                          (val (%true)))
                 (if (%null? expr)
                     val
                     (let ((v (%eval (%car expr) env)))
                       (if (%true? v)
                           (loop (%cdr expr) v)
                           (%false)))))))

;;; begin
;;; ----------------------------------------------------------------------

(%defspecial 'begin
             (lambda (expr env) 
               (let loop ((forms (%cdr expr))
                          (val (%nothing)))
                 (if (%null? forms)
                     val
                     (let ((form (%car forms)))
                       (loop (%cdr forms)
                             (%eval form env)))))))

;;; define
;;; ----------------------------------------------------------------------

(%defspecial 'define
             (lambda (expr env)
               (%defglobal (%list-ref expr 1) (%eval (%list-ref expr 2) env))
               (%list-ref expr 1)))

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

;;; if
;;; ----------------------------------------------------------------------

(%defspecial 'if
             (lambda (expr env)
               (let ((test (%list-ref expr 1))
                     (conseq (%list-ref expr 2))
                     (alt? (> (%length expr) 3)))
                 (if (%eval test env)
                     (%eval conseq env)
                     (if alt?
                         (%eval (%list-ref expr 3) env)
                         (%nothing))))))

;;; let
;;; ----------------------------------------------------------------------

(define (%eval-let expr env)
  (let ((body (%cons 'begin (%drop 2 expr))))
    (let loop ((bindings (%list-ref expr 1))
               (env env))
      (if (%null? bindings)
          (%eval body env)
          (let ((binding (%car bindings)))
            (loop (%cdr bindings)
                  (%add-binding env
                                (%car binding)
                                (%eval (%car (%cdr binding))
                                       env))))))))

(%defspecial 'let (lambda (expr env)(%eval-let expr env)))

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
               (if (%eval (%car (%cdr expr)) env)
                   (%false)
                   (%true))))

;;; or
;;; ----------------------------------------------------------------------

(%defspecial 'or
             (lambda (expr env)
               (let loop ((expr (%cdr expr)))
                 (if (%null? expr)
                     (%false)
                     (let ((v (%eval (%car expr) env)))
                       (if v
                           v
                           (loop (%cdr expr))))))))

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
               (let ((form (%cons 'begin (%cdr expr))))
                 (let loop ()
                   (%eval form env)
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


