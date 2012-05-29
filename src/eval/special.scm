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
  (let ((evaluator (table-ref $special-forms-table (%car expr) #f)))
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

(define (%define-function-parameter->formal-parameter arg)
  (cond
   ((symbol? arg) arg)
   ((%list? arg) (%car arg))
   (else (error (string-append "Invalid function parameter: " (%as-string arg))))))

(define (%define-function-parameter->type env arg)
  (cond
   ((symbol? arg) Anything)
   ((%list? arg) (%eval (%cadr arg) env))
   (else (error (string-append "Invalid function parameter: " (%as-string arg))))))

(define (%parse-define-function-form f env)
  (let* ((proto (%list-ref f 1))
         (body (%cons 'begin (%drop 2 f)))
         (fname (%list-ref body 0))
         (fn (table-ref $bard-global-variables fname #f))
         (args (%drop 1 body))
         (amp-pos (%position (lambda (a)(eq? '& a)) args))
         (required-args (if (%true? amp-pos)
                            (%take amp-pos args)
                            args))
         (rest-arg (if (%true? amp-pos)
                       (%car (%drop (+ amp-pos 1) args))
                       #f))
         (formals (%append (%map %define-function-parameter->formal-parameter required-args)
                           (if (%true? rest-arg) (%list rest-arg) %nil)))
         (signature (%append (%map (partial %define-function-parameter->type env) required-args)
                             (if (%true? rest-arg) (%list &) %nil))))
    (%list fname fn env formals body signature)))

(define (%fdesc-get-function-name fdesc)(%list-ref fdesc 0))
(define (%fdesc-get-function fdesc)(%list-ref fdesc 1))
(define (%fdesc-get-method-environment fdesc)(%list-ref fdesc 2))
(define (%fdesc-get-method-formals fdesc)(%list-ref fdesc 3))
(define (%fdesc-get-method-body fdesc)(%list-ref fdesc 4))
(define (%fdesc-get-method-signature fdesc)(%list-ref fdesc 5))

(%defspecial 'define-function
             (lambda (expr env)
               (let* ((fdesc (%parse-define-function-form (%cdr expr) env))
                      (fname (%fdesc-get-function-name fdesc))
                      (menv (%fdesc-get-method-environment fdesc))
                      (mformals (%fdesc-get-method-formals fdesc))
                      (mbody (%fdesc-get-method-body fdesc))
                      (fn (or (%fdesc-get-function fdesc)
                              (%make-function name: fname)))
                      (msig (%fdesc-get-method-signature fdesc))
                      (method (%make-interpreted-method mformals mbody environment: menv name: fname)))
                 (%add-method! fn msig method))))

;;; function
;;; ----------------------------------------------------------------------

(%defspecial 'function
             (lambda (expr env)
               (if (> (%length expr) 1)
                   (%make-function name: (%list-ref expr 1))
                   (%make-function))))

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

(%defspecial 'let
             (lambda (expr env)
               (let ((body (%drop 2 expr)))
                 (let loop ((bindings (%list-ref expr 1))
                            (env env))
                   (if (%null? bindings)
                       (let loop2 ((forms body)
                                   (val (%nothing)))
                         (if (%null? forms)
                             val
                             (let ((form (%car forms)))
                               (loop2 (%cdr forms)
                                      (%eval form env)))))
                       (let ((binding (%car bindings)))
                         (loop (%cdr bindings)
                               (%add-binding env
                                             (%car binding)
                                             (%eval (%car (%cdr binding))
                                                    env)))))))))

;;; method
;;; ----------------------------------------------------------------------

(define (%parse-method-form m)
  (let* ((first (%list-ref m 0))
         (mname (if (symbol? first)
                    first
                    #f))
         (formals (if (symbol? first)
                      (%list-ref m 1)
                      (%list-ref m 0)))
         (body (if (symbol? first)
                   (%drop 2 m)
                   (%drop 1 m))))
    (%list mname formals body)))

(define (%mdesc-get-name mdesc)(%list-ref fdesc 0))
(define (%mdesc-get-formals mdesc)(%list-ref fdesc 1))
(define (%mdesc-get-body mdesc)(%list-ref fdesc 2))

(%defspecial 'method
             (lambda (expr env)
               (let* ((mdesc (%parse-method-form (%cdr expr)))
                      (mname (%mdesc-get-name mdesc))
                      (formals (%mdesc-get-formals mdesc))
                      (body (%mdesc-get-body mdesc)))
                 (%make-interpreted-method formals body environment: env name: mname))))

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

(%defspecial 'quote (lambda (expr env)
                      (if (= 2 (%length expr))
                          (%car (%cdr expr))
                          (error (string-append "Wrong number of arguments to quote: " (%as-string (%cdr expr)))))))

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
