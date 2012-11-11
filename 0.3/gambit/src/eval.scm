;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          eval.scm
;;;; Project:       Bard
;;;; Purpose:       the bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the execution model is this:
;;; 1. compile an expression in the current environment,
;;;    yielding a list of primitive forms ("instructions')
;;; 2. execute the list of instructions, yielding a set 
;;;    of outputs
;;;
;;; an instruction is a form like:
;;; (OP arg1 arg2 ...)
;;;
;;; OP is a symbol that identifies an execution procedure.
;;; loading replaces each OP with the execution
;;; procedure it refers to. %exec applies each execution
;;; procedure to its arguments.
;;;
;;; a list of instructions can be converted to serialized 
;;; form for storage, and can be reconstituted from
;;; storage by deserialization, which enables it to be
;;; executed. That's how we compile files: by compiling
;;; the expressions they contain and serializing the
;;; results to an object file.
;;;
;;; we can also serialize the toplevel environment.
;;; serializing the environment (including modules and
;;; execution stack) yields a bard image that can be
;;; loaded and resumed.

;;; ---------------------------------------------------------------------
;;; compiler utils
;;; ---------------------------------------------------------------------

;;; TODO: replace with a proper implementation
(define (%make-method-function params body env)
  `(method ,params ,body ,env))

;;; ---------------------------------------------------------------------
;;; self-evaluating expressions
;;; ---------------------------------------------------------------------

(define (%self-evaluating? expr)
  (or (null? expr)
      (and (not (pair? expr))
           (not (symbol? expr)))))

(define (%compile-self-evaluating expr env) 
  `(CODE (CONST ,expr)))

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

(define $special-forms-table (make-table test: eq?))

(define (%define-special-form form-name compiler-fn)
  (table-set! $special-forms-table form-name compiler-fn))

(define (%special-form? form)
  (and (pair? form)
       (table-ref $special-forms-table (car form) #f)))

(define (%compile-lambda expr env)
  (let ((params (cadr expr))
        (body (cddr expr)))
    `(CODE (CONST ,(%make-method-function params body env)))))

(define (%compile-sequence exprs env)
  `(CODE (SEQ ,@(map (lambda (e)(%compile-expression e env)) exprs))))

(define (%compile-begin expr env)
  (%compile-sequence (cdr expr) env))

(define (%compile-cond expr env)
  (let* ((clauses (cdr expr))
         (compiled-clauses (map (lambda (c)
                                  (list (if (eq? (car c) 'else)
                                            (%compile-expression #t env)
                                            (%compile-expression (car c) env))
                                        (%compile-sequence (cdr c) env)))
                                clauses)))
    `(CODE (COND ,@compiled-clauses))))

(define (%compile-define-class expr env)
  (let ((classname (list-ref expr 2)))
    `(CODE (DEF ,classname ,(%make-class classname) ,env))))

(define (%compile-define-macro expr env) 
  (let ((prototype (list-ref expr 2))
        (expansion (cdddr expr)))
    `(CODE (MACRO ,prototype ,@expansion))))

(define (%param->formal exp)
  (if (symbol? exp)
      exp
      (if (pair? exp)
          (car exp)
          (error (str "invalid method parameter: " exp)))))

(define (%param->type exp)
  (cond
   ((symbol? exp) 'Anything)
   ((pair? exp)(let ((exp-type (cadr exp)))
                 (cond
                  ((symbol? exp-type) exp-type)
                  ((and (pair? exp-type)
                        (eq? 'singleton (car exp-type))) exp-type)
                  (else (error (str "invalid method parameter: " exp))))))
   (else (error (str "invalid method parameter: " exp)))))

(define (%compile-define-method expr env)
  (let* ((prototype (list-ref expr 2))
         (fname (car prototype))
         (params (cdr prototype))
         (formals (map %param->formal params))
         (types (map %param->type params))
         (body (cdddr expr))
         (method-fn (%make-method-function formals body env)))
    `(CODE (DEFMETHOD ',fname ',types ,method-fn))))

(define (%parse-protocol-function expr)
  (let* ((fproto (list-ref expr 0))
         (arrow (list-ref expr 1))
         (fname (car fproto))
         (in-types (cdr fproto))
         (out-types (list-ref expr 2)))
    (if (eq? '-> arrow)
        `(,fname ,in-types ,out-types)
        (error (str "Malformed protocol function: " expr)))))

(define (%compile-define-protocol expr env)
  (let* ((pname (list-ref expr 2))
         (fprotos (map %parse-protocol-function (cdddr expr))))
    `(CODE (DEFPROTOCOL ',pname ',fprotos))))

(define (%parse-slot-spec spec)
  (let* ((spec (cond
                ((symbol? spec)(list spec))
                ((pair? spec) spec)
                (else (error "Malformed slot spec: " spec))))
         (slotname (car spec))
         (default (let ((found (memq default: (cdr spec))))
                    (if found
                        (cadr found)
                        (%nothing))))
         (mutable? (let ((found (memq mutable: (cdr spec))))
                    (if found
                        (cadr found)
                        (%false)))))
    `(,slotname ,default ,mutable?)))

(define (%compile-define-record expr env) 
  (let ((rname (list-ref expr 2))
        (includes (list-ref expr 3))
        (slotspecs (map %parse-slot-spec (cddddr expr))))
    `(CODE (DEFRECORD ',rname ',includes ',slotspecs))))

(define (%compile-define-variable expr env)
  (let* ((vname (list-ref expr 2))
        (val-expr (list-ref expr 3))
        (val (%compile-expression val-expr env)))
    `(CODE (DEF ',vname ,val))))


(define (%compile-define-vector expr env) 
  (let* ((vname (list-ref expr 2))
         (params (cdddr expr))
         (count (let ((found (memq count: params)))
                  (if found
                      (cadr found)
                      #f)))
         (min (let ((found (memq minimum-count: params)))
                (if found
                    (cadr found)
                    #f)))
         (max (let ((found (memq maximum-count: params)))
                (if found
                    (cadr found)
                    #f)))
         (type (let ((found (memq type: params)))
                 (if found
                     (cadr found)
                     'Anything))))
    (if count
        (if min
            (error (str "found count: but also minimum-count:"))))
    (if count
        (if max
            (error (str "found count: but also maximum-count:"))))
    (if min
        (if count
            (error (str "found minimum-count: but also count:"))))
    (if max
        (if count
            (error (str "found minimum-count: but also count:"))))
    (let ((min (or min count 0))
          (max (or max count #f)))
      `(CODE (DEFVECTOR ',vname ,min ,max ,type)))))

(define (%compile-define expr env)
  (let ((key (cadr expr)))
    (case key
      ;; define class
      ((class) (%compile-define-class expr env))
      ;; define macro
      ((macro) (%compile-define-macro expr env))
      ;; define method
      ((method) (%compile-define-method expr env))
      ;; define protocol
      ((protocol) (%compile-define-protocol expr env))
      ;; define record
      ((record) (%compile-define-record expr env))
      ;; define variable
      ((variable) (%compile-define-variable expr env))
      ;; define vector
      ((vector) (%compile-define-vector expr env))
      (else (error (str "Unrecognized defining form: " key))))))

(define (%compile-ensure expr env) #f)

(define (%compile-if expr env) 
  (let ((test (list-ref expr 1))
        (consequent (list-ref expr 2))
        (alternate (if (> (length expr) 3)
                       (list-ref expr 3)
                       (%nothing))))
    (%compile-cond `(cond (,test ,consequent)(else ,alternate)) env)))

(define (%compile-let expr env) #f)
(define (%compile-loop expr env) #f)
(define (%compile-match expr env) #f)
(define (%compile-quasiquote expr env) #f)

(define (%compile-quote expr env)
  `(CODE (CONST ,(cadr expr))))

(define (%compile-unless expr env) 
  (let ((test (list-ref expr 1))
        (consequent (%nothing))
        (alternate (cddr expr)))
    (%compile-cond `(cond (,test ,@consequent)(else ,@alternate)) env)))

(define (%compile-unquote expr env) #f)
(define (%compile-unquote-splicing expr env) #f)

(define (%compile-when expr env) 
  (let ((test (list-ref expr 1))
        (consequent (cddr expr))
        (alternate (%nothing)))
    (%compile-cond `(cond (,test ,@consequent)(else ,@alternate)) env)))

(define (%compile-with-exit expr env) #f)

(%define-special-form 'λ %compile-lambda)
(%define-special-form '^ %compile-lambda)
(%define-special-form 'lambda %compile-lambda)
(%define-special-form 'method %compile-lambda)
(%define-special-form 'begin %compile-begin)
(%define-special-form 'cond %compile-cond)
(%define-special-form 'define %compile-define)
(%define-special-form 'ensure %compile-ensure)
(%define-special-form 'if %compile-if)
(%define-special-form 'let %compile-let)
(%define-special-form 'loop %compile-loop)
(%define-special-form 'match %compile-match)
(%define-special-form 'quasiquote %compile-quasiquote)
(%define-special-form 'quote %compile-quote)
(%define-special-form 'unless %compile-unless)
(%define-special-form 'unquote %compile-unquote)
(%define-special-form 'unquote-splicing %compile-unquote-splicing)
(%define-special-form 'when %compile-when)
(%define-special-form 'with-exit %compile-with-exit)

(define (%compile-special-form expr env)
  (let ((comp (table-ref $special-forms-table (car expr))))
    (comp expr env)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define $macro-forms-table (make-table test: eq?))

(define (%define-macro-form form-name expander-fn)
  (table-set! $macro-forms-table form-name expander-fn))

(define (%macro-form? form)
  (and (pair? form)
       (table-ref $macro-forms-table (car form) #f)))


;;; ---------------------------------------------------------------------
;;; app forms
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; execution of compiled expressions
;;; ---------------------------------------------------------------------

(define (%exec expr env)
  #f)

;;; ---------------------------------------------------------------------
;;; evaluation
;;; ---------------------------------------------------------------------

(define (%compile-expression expr #!optional (env '()))
  (if (%self-evaluating? expr)
      (%compile-self-evaluating expr env)
      (cond
       ((%special-form? expr)(%compile-special-form expr env))
       ((%macro-form? expr)(%compile-expression (%macroexpand expr) env))
       (else (%compile-app-form expr env)))))

(define (%eval expr #!optional (env '()))
  (%exec (%compile-expression expr env)))

;;; (define $env '())
;;; (%compile-expression '() $env)
;;; (%compile-expression 1.2 $env)
;;; (%compile-expression #f $env)
;;; (%compile-expression "Foo" $env)
;;; (%compile-expression '(^ (x) (* x x)) $env)
;;; (%compile-expression '(begin 1 2 3) $env)
;;; (%compile-expression '(cond (1 1)(2 2)) $env)
;;; (%compile-expression '(define class Point) $env)
;;; (%compile-expression '(define macro (and a b) `(if ,a (if ,b ,b false) false)) $env)
;;; (%compile-expression '(define method (add a b) (= a b)) $env)
;;; (%compile-expression '(define method (add (a <fixnum>) (b <fixnum>)) (= a b)) $env)
;;; (%compile-expression '(define protocol Ratio ((num Ratio) -> (Integer)) ((denom Ratio) -> (Integer))) $env)
;;; (%compile-expression '(define record Point () x y) $env)
;;; (%compile-expression '(define record Point3D (Point) (z default: 0 mutable: true)) $env)
;;; (%compile-expression '(define variable Foo 0) $env)
;;; (%compile-expression '(define vector Flags count: 8 type: '<bit>) '())
;;; (%compile-expression '(define vector OSType count: 4 type: '<word8>) '())
;;; (%compile-expression '(define vector Cons count: 2 type: 'Anything) '())
;;; (%compile-expression '(define vector Stack minimum-count: 0 maximum-count: false type: 'Anything) '())
;;; (%compile-expression '(if 1 1 2) $env)
;;; (%compile-expression '(quote x) $env)
;;; (%compile-expression '(unless 1 1 2) $env)
;;; (%compile-expression '(when 1 1 2) $env)
