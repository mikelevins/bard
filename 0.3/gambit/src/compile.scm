;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; discriminating expression types
;;; ---------------------------------------------------------------------

(define (%self-evaluating? expr)
  (and (not (symbol? expr))
       (not (pair? expr))))

(define (%special-form? expr)
  (memq expr '(λ ^ lambda method begin cond define ensure if let loop
                 match quasiquote quote setter set! unless unquote
                 unquote-splicing when with-exit)))

(define (%macro-form? expr)
  #f)

(define (%lambda-expression? expr)
  (and (pair? expr)
       (memq (car expr) '(λ ^ lambda method))))

;;; ---------------------------------------------------------------------
;;; code generators
;;; ---------------------------------------------------------------------

(define (%seq . code)
  (apply append code))

(define (%gen opcode . args)
  (list (cons opcode args)))

;;; this generator handles ensuring that we know how many arguments
;;; and what argument names a compiled method expects, and arranges to
;;; extend the method environment to accommodte them. how do we know
;;; how many restargs to collect? the app compiler counts the args
;;; passed in the application and passes that sum as an argument to
;;; CALL the legal lambda list forms for methods are:
;;; (^ () ...)
;;; (^ args ...)
;;; (^ (a b ... n))
;;; (^ (a b & rest))
(define (%gen-args arglist)
  (cond
   ((null? arglist)(%gen 'ARGS 0))
   ((symbol? arglist)(%gen 'ARGS. 0))
   ((pair? arglist)(let ((restpos (position '& arglist)))
                     (if restpos
                         (%gen 'ARGS. restpos)
                         (%gen 'ARGS (length arglist)))))
   (else (error (str "Invalid method argument list: " arglist)))))

(define %next-label-number #f)
(let ((_labelnum 0))
  (set! %next-label-number
        (lambda ()
          (set! _labelnum (+ 1 _labelnum))
          _labelnum)))

(define (%gen-label #!optional (name 'L))
  (string->symbol (str name (%next-label-number))))

(define (%gen-none) '())

(define (%gen-var expr env) 
  (receive (i j)(%find-variable env expr)
           (if i
               (%gen 'LREF i j "; " expr)
               (%gen 'GREF expr))))

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

(define (%compile-not-yet-implemented name expr env val? more?)
  (error (str "Compiler not yet implemented for " name)))

(define (%compile-application fexpr argexprs env val? more?)
  (let ((prim (%primitive? fexpr)))
    (cond
     (prim (if (and (not val?)(not (%prim-side-effects? prim)))
               ;; side-effect free primitive call, value not needed
               (%compile-begin argexprs env #f more?)
               ;; value or side-effect needed
               (%seq (%compile-list argexprs env val? more?)
                     (%gen 'PRIM (%prim-opname prim)(length argexprs))
                     (if val? '() (%gen 'POP))
                     (if more? '() (%gen 'RETURN)))))
     ((and (%lambda-expression? fexpr)
           (null? (cadr fexpr)))
      ;; (lambda () . body) -> (begin . body)
      (assert (null? argexprs) "Too many arguments")
      (%compile-begin (drop 2 fexpr) env val? more?))
     (more? ;; must save the return continuation
      (let ((k (%gen-label 'k)))
        (%seq (%gen 'SAVE k)
              (%compile-list argexprs env val? more?)
              (%compile fexpr env #t #t)
              (%gen 'CALL (length argexprs))
              (list k)
              (if val? '() (%gen 'POP)))))
     (else ;; the function call is a simple jump 
      (%seq (%compile-list argexprs env val? more?)
            (%compile fexpr env #t #t)
            (%gen 'CALL (length argexprs)))))))

(define (%compile-begin exprs env val? more?)
  (cond
   ((null? exprs)(%compile-constant '() env val? more?))
   ((= 1 (length exprs))(%compile (car exprs) env val? more?))
   (else (%seq (%compile (car exprs) env #f #t)
               (%compile-begin (cdr exprs) env val? more?)))))


(define (%cond-clauses->if expr)
  (if (null? expr)
      '()
      (if (null? (cdr expr))
          `(if ,(caar expr) (begin ,@(cdar expr)))
          (let* ((c1 (car expr))
                 (test (car c1))
                 (then `(begin ,@(cdr c1)))
                 (more (%cond-clauses->if (cdr expr))))
            `(if ,test ,then ,more)))))

(define (%compile-cond expr env val? more?)
  (%compile (%cond-clauses->if expr) env val? more?))

(define (%compile-constant expr env val? more?)
  (if val?
      (if more?
          (%gen 'CONST expr)
          (%seq (%gen 'CONST expr)
                (%gen 'RETURN)))
      (%gen-none)))

(define (%compile-define-class expr env val? more?)
  (let ((cname (car expr)))
    (assert (symbol? cname)
            (str "Non-symbol given as a class name in define class: " cname))
    (%seq (%gen 'CLASS cname)
          (if val? '() (%gen 'POP))
          (if more? '() (%gen 'RETURN)))))

(define (%compile-define-macro expr env val? more?)
  (%compile-not-yet-implemented '(define macro) expr env val? more?))

(define (%compile-define-method expr env val? more?)
  (%compile-not-yet-implemented '(define method) expr env val? more?))

(define (%compile-define-protocol expr env val? more?)
  (%compile-not-yet-implemented '(define protocol) expr env val? more?))

(define (%compile-define-record expr env val? more?)
  (%compile-not-yet-implemented '(define record) expr env val? more?))

(define (%compile-define-variable expr env val? more?)
  (let ((varname (car expr))
        (valexpr (cadr expr)))
    (assert (symbol? varname)
            (str "Non-symbol given as a variable name in define variable: " varname))
    (%seq (%compile valexpr env #t #t)
          (%gen 'DEF varname)
          (if val? '() (%gen 'POP))
          (if more? '() (%gen 'RETURN)))))

(define (%compile-define-vector expr env val? more?)
  (%compile-not-yet-implemented '(define vector) expr env val? more?))

(define (%compile-define expr env val? more?)
  (case (cadr expr)
    ((class)(%compile-define-class (cddr expr) env val? more?))
    ((macro)(%compile-define-macro (cddr expr) env val? more?))
    ((method)(%compile-define-method (cddr expr) env val? more?))
    ((protocol)(%compile-define-protocol (cddr expr) env val? more?))
    ((record)(%compile-define-record (cddr expr) env val? more?))
    ((variable)(%compile-define-variable (cddr expr) env val? more?))
    ((vector)(%compile-define-vector (cddr expr) env val? more?))
    (else (error (str "Unrecognized definition: define" (cadr expr))))))

(define (%compile-ensure expr env val? more?)
  (%compile-not-yet-implemented 'ensure expr env val? more?))

(define (%compile-if test then else env val? more?)
  (let* ((testcode (%compile test env #t #t))
         (thencode (%compile then env val? more?))
         (elsecode (%compile else env val? more?)))
    (cond
     ((equal? thencode elsecode)(%seq (%compile test env #f #t)
                                      elsecode))
     ((null? thencode)(let ((L2 (%gen-label)))
                        (%seq testcode
                              (%gen 'TJUMP L2)
                              elsecode
                              (list L2)
                              (if more? '() (%gen 'RETURN)))))
     ((null? elsecode)(let ((L1 (%gen-label)))
                        (%seq testcode
                              (%gen 'FJUMP L1)
                              thencode
                              (if more? '() (%gen 'RETURN)))))
     (else (let ((L1 (%gen-label))
                 (L2 (if more? (%gen-label) '())))
             (%seq testcode
                   (%gen 'FJUMP L1)
                   thencode
                   (%gen 'JUMP L2)
                   (list L1)
                   elsecode
                   (%gen 'JUMP L2)
                   (list L2)))))))

(define (%compile-let expr env val? more?)
  (%compile-not-yet-implemented 'let expr env val? more?))

(define (%compile-list expr env val? more?)
  (if (null? expr)
      '()
      (%seq (%compile (car expr) env #t #t)
            (%compile-list (cdr expr) env val? more?))))

(define (%compile-loop expr env val? more?)
  (%compile-not-yet-implemented 'loop expr env val? more?))

(define (%compile-match expr env val? more?)
  (%compile-not-yet-implemented 'match expr env val? more?))

;;; compile-method
;;; in order to compile a method form we must do several things:
;;; 1. parse the formal parameters into an environment frame that can
;;;    be used to construct the compile-time environment so that
;;;    references to the formal parameters are treated as references to
;;;    lexical variables
;;; 2. store the constructed compile-time environment frame so that
;;;    when the method is actually called, we can augment the actual
;;;    runtime environment with bindings of the formal parameters to
;;;    the passed parameters
;;; 3. compile the body of the method in the compile-time environment,
;;;    generating code at the beginning of the method body that
;;;    replaces the top frame of the method environment with a frame
;;;    constructed by binding the actual parameters to the arguments
;;;    passed in the method call.
;;; 
;;; method parameter lists can take any of the following forms:
;;;
;;; () ; no arguments accepted
;;; args ; any and all arguments bound as a list to args
;;; (& args)  ; any and all arguments bound as a list to args
;;; (& {key1: val1 key2: val2 ...}) ; any and all args treated as 
;;;                                 ; a plist of keyword arguments with
;;;                                 ; default values
;;; (arg1 arg2 ...) ; required arguments
;;; (arg1 arg2 ... & more) ; required arguments plus rest arguments bound to more
;;; (arg1 arg2 ... & {key1: val1 key2: val2 ...}) ; required arguments plus
;;;                                               ; keyword arguments with default values

(define (%&-position params)
  (position '& params test: eq?))

(define (%method-parameters->env-frame params env)
  (cond
   ((null? params)(%empty-frame))
   ((%&-position params)(error (str "rest arguments are not yet implemented: " params)))
   ((every? symbol? params)(%make-frame params))
   (else (error (str "Invalid parameter list: " params)))))

(define (%compile-method params body env val? more?)
  (let ((menv (%add-frame env (%method-parameters->env-frame params env))))
    `((CONST ,(%makefn env: menv parameters: params 
                      code: (%seq (%gen-args params)
                                  (%compile-begin body menv #t #f)))))))

(define (%compile-quasiquote expr env val? more?)
  (%compile-not-yet-implemented 'quasiquote expr env val? more?))

(define (%compile-quote expr env val? more?)
  (%compile-constant expr env val? more?))

(define (%compile-setter expr env val? more?)
  (%compile-not-yet-implemented 'setter expr env val? more?))

(define (%compile-set! expr env val? more?)
  (%compile-not-yet-implemented 'set! expr env val? more?))

(define (%compile-unquote expr env val? more?)
  (%compile-not-yet-implemented 'unquote expr env val? more?))

(define (%compile-unquote-splicing expr env val? more?)
  (%compile-not-yet-implemented 'unquote-splicing expr env val? more?))

(define (%compile-variable expr env val? more?)
  (if val? 
      (%seq (%gen-var expr env)
            (if more? '() (%gen 'RETURN)))
      '()))

(define (%compile-with-exit expr env val? more?)
  (%compile-not-yet-implemented 'with-exit expr env val? more?))

(define (%compile-special-form expr env val? more?)
  (case (car expr)
    ((λ ^ lambda method)(%compile-method (cadr expr) (cddr expr) env val? more?))
    ((begin)(%compile-begin (cdr expr) env val? more?))
    ((cond)(%compile-cond (cdr expr) env val? more?))
    ((define)(%compile-define expr env val? more?))
    ((ensure)(%compile-ensure expr env val? more?))
    ((if)(%compile-if (list-ref expr 1)
                      (list-ref expr 2)
                      (if (null? (drop 3 expr))
                          '()
                          (list-ref expr 3))
                      env val? more?))
    ((let)(%compile-let expr env val? more?))
    ((loop)(%compile-loop expr env val? more?))
    ((match)(%compile-match expr env val? more?))
    ((quasiquote)(%compile-quasiquote expr env val? more?))
    ((quote)(%compile-quote (cadr expr) env val? more?))
    ((setter)(%compile-setter expr env val? more?))
    ((set!)(%compile-set! expr env val? more?))
    ((unless)(%compile-if (list 'not
                                (list-ref expr 1))
                          (cons 'begin (drop 2 expr))
                          '()
                          env val? more?))
    ((unquote)(%compile-unquote expr env val? more?))
    ((unquote-splicing)(%compile-unquote-splicing expr env val? more?))
    ((when)(%compile-if (list-ref expr 1)
                        (cons 'begin (drop 2 expr))
                        '()
                        env val? more?))
    ((with-exit)(%compile-with-exit expr env val? more?))
    (else (error (str "Unrecognized special form: " (car expr))))))



;;; ---------------------------------------------------------------------
;;; main compiler entry point
;;; ---------------------------------------------------------------------

(define (%compile expr env val? more?)
  (cond
   ((%self-evaluating? expr)(%compile-constant expr env val? more?))
   ((symbol? expr)(%compile-variable expr env val? more?))
   ((%special-form? (car expr))(%compile-special-form  expr env val? more?))
   ((%macro-form? expr)(%compile  (%macroexpand expr) env val? more?))
   (else (%compile-application (car expr) (cdr expr) env val? more?))))


