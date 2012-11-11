;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; borrowing from the structure of the Larceny compiler, we have the
;;; following compiler passes:
;;; 1. standardize syntax
;;; 2. optimize syntax
;;; 3. generate abstract code
;;; 4. perform local optimizations
;;; 5. generate relocatable bytecode
;;; 6. load-time linking for the VM

;;; ---------------------------------------------------------------------
;;; macroexpanders
;;; ---------------------------------------------------------------------

(define $macroxpanders (make-table test: eq?))

(define (%set-macroexpander-for! mname mfun)
  (table-set! $macroxpanders mname mfun))

(define (%macroexpander-for mname)
  (table-ref $macroxpanders mname #f))

(%set-macroexpander-for! 'and
                         (lambda (exp)
                           (let* ((sval (%syntax-value exp))
                                  (opform (car sval))
                                  (argforms (cdr sval)))
                             (if (null? argforms)
                                 `(syntax:true #t)
                                 (let* ((test1 (car argforms))
                                        (more-tests (cdr argforms))
                                        (then (%macroexpand `(syntax:app 
                                                              ((syntax:symbol and)
                                                               ,@more-tests)))))
                                   `(syntax:app
                                     ((syntax:symbol:if)
                                      ,test1
                                      ,then
                                      (syntax:false #f))))))))

;;; ---------------------------------------------------------------------
;;; 1. standardize syntax
;;; ---------------------------------------------------------------------

(define (%syntax-type expr)
  (car expr))

(define (%syntax-value expr)
  (cadr expr))

(define (%macro-form? expr)
  (and (eq? 'syntax:app (%syntax-type expr))
       (eq? 'syntax:symbol (%syntax-type (car (%syntax-value expr))))
       (and (%macroexpander-for (%syntax-value (car (%syntax-value expr))))
            #t)))

(define (%macroexpand expr)
  (let ((_expand (%macroexpander-for (%syntax-value (car (%syntax-value expr))))))
    (if _expand
        (_expand expr)
        (error (str "Undefined macro " (%syntax-value (car (%syntax-value expr))))))))

;;; the only change we make in this pass is macroexpansion
(define (%expand-app-syntax expr)
  (if (%macro-form? expr)
      (%macroexpand expr)
      expr))

(define (%expand-character-syntax expr)
  expr)

(define (%expand-false-syntax expr)
  expr)

(define (%expand-list-syntax expr)
  expr)

(define (%expand-method-syntax expr)
  expr)

(define (%expand-nothing-syntax expr)
  expr)

(define (%expand-bignum-syntax expr)
  expr)

(define (%expand-fixnum-syntax expr)
  expr)

(define (%expand-flonum-syntax expr)
  expr)

(define (%expand-ratnum-syntax expr)
  expr)

(define (%expand-self-evaluating-syntax expr)
  expr)

(define (%expand-series-expression-syntax expr)
  expr)

(define (%expand-table-syntax expr)
  expr)

(define (%expand-true-syntax expr)
  expr)

(define (%expand-undefined-syntax expr)
  expr)

(define (%expand-variable-reference-syntax expr)
  expr)

(define (%get-syntax-expander-for stype)
  (case stype
    ((syntax:app) %expand-app-syntax)
    ((syntax:character) %expand-character-syntax)
    ((syntax:empty-app) %expand-nothing-syntax)
    ((syntax:empty-list) %expand-nothing-syntax)
    ((syntax:empty-string) %expand-nothing-syntax)
    ((syntax:empty-table) %expand-nothing-syntax)
    ((syntax:empty-series) %expand-nothing-syntax)
    ((syntax:false) %expand-false-syntax)
    ((syntax:keyword) %expand-self-evaluating-syntax)
    ((syntax:list) %expand-list-syntax)
    ((syntax:method) %expand-method-syntax)
    ((syntax:nothing) %expand-nothing-syntax)
    ((syntax:bignum) %expand-bignum-syntax)
    ((syntax:fixnum) %expand-fixnum-syntax)
    ((syntax:flonum) %expand-flonum-syntax)
    ((syntax:ratnum) %expand-ratnum-syntax)
    ((syntax:self-evaluating) %expand-self-evaluating-syntax)
    ((syntax:series) %expand-series-expression-syntax)
    ((syntax:symbol) %expand-variable-reference-syntax)
    ((syntax:table) %expand-table-syntax)
    ((syntax:text) %expand-self-evaluating-syntax)
    ((syntax:true) %expand-true-syntax)
    ((syntax:undefined) %expand-undefined-syntax)
    (else #f)))

(define (%expand-syntax expr)
  (let ((_expand (%get-syntax-expander-for (%syntax-type expr)))) 
    (if _expand
        (_expand expr)
        (error (str "Syntax error: " expr)))))

;;; (%expand-syntax (bard:read-from-string "#\\C"))
;;; (%expand-syntax (bard:read-from-string "false"))
;;; (%expand-syntax (bard:read-from-string "Foo:"))
;;; (%expand-syntax (bard:read-from-string "nothing"))
;;; (%expand-syntax (bard:read-from-string "()"))
;;; (%expand-syntax (bard:read-from-string "[]"))
;;; (%expand-syntax (bard:read-from-string "{}"))
;;; (%expand-syntax (bard:read-from-string "\"\""))
;;; (%expand-syntax (bard:read-from-string "[0 1 2 3]"))
;;; (%expand-syntax (bard:read-from-string "99999999999999999999999999999999999999999999999999999999999"))
;;; (%expand-syntax (bard:read-from-string "999"))
;;; (%expand-syntax (bard:read-from-string "9.99"))
;;; (%expand-syntax (bard:read-from-string "1.3e+12"))
;;; (%expand-syntax (bard:read-from-string "2/3"))
;;; (%expand-syntax (bard:read-from-string "|Foo Bar|"))
;;; (%expand-syntax (bard:read-from-string "{a: 1 b: 2}"))
;;; (%expand-syntax (bard:read-from-string "\"Foo bar baz\""))
;;; (%expand-syntax (bard:read-from-string "undefined"))
;;; (%expand-syntax (bard:read-from-string "(and true false )"))
;;; (%expand-syntax (bard:read-from-string "(and (> 2 3) (< 2 3) )"))



;;; ---------------------------------------------------------------------
;;; 2. optimize syntax
;;; ---------------------------------------------------------------------

(define (%optimize-syntax standardized-expr env)
  standardized-expr)

;;; ---------------------------------------------------------------------
;;; 3. generate abstract code
;;; ---------------------------------------------------------------------

(define (%gen-app expr env)
  `(bogus app code for ,expr))

(define (%gen-const k)
  `((VAL ,k)))

(define (%gen-list lexpr env)
  (let* ((elts (map (lambda (lx)(%gen lx env))
                   lexpr))
         (eltcount (length elts)))
    (append (apply append elts)
            (list `(LIST ,eltcount)))))

(define (%lambda-list-syntax->lambda-list lambda-list-expr)
  (map %syntax-value (%syntax-value lambda-list-expr)))

(define (%lambda-list->env lambda-list env)
  (%add-frame env (%make-frame (map (lambda (p)(%make-lvar p #!unbound #f)) lambda-list))))

(define (%gen-method-body body-expr env)
  (apply append (map (lambda (x)(%gen x env)) body-expr)))

(define (%gen-method mexpr env)
  (let* ((lambda-list-expr (car mexpr))
         (lambda-list (%lambda-list-syntax->lambda-list lambda-list-expr))
         (menv (%lambda-list->env lambda-list env))
         (body-expr (cdr mexpr))
         (body (%gen-method-body body-expr menv)))
    `((VAL ,menv)(VAL ,lambda-list)(VAL ,body)(METHOD))))

(define (%gen-series sexpr env)
  `(bogus series code for ,sexpr))

(define (%gen-variable-reference vnm env)
  (let ((vref (%find-var-in-env vnm env)))
    (if vref
        `((LREF ,(car vref) ,(cdr vref)))
        `((GREF ,vnm)))))

(define (%gen-table texpr env)
  `(bogus table code for ,texpr))


(define (%gen expr env)
  (case (%syntax-type expr)
    ((syntax:app) (%gen-app expr env))
    ((syntax:character) (%gen-const (%syntax-value expr)))
    ((syntax:empty-app) (%gen-const (%nothing)))
    ((syntax:empty-list) (%gen-const (%nothing)))
    ((syntax:empty-string) (%gen-const (%nothing)))
    ((syntax:empty-table) (%gen-const (%nothing)))
    ((syntax:empty-series) (%gen-const (%nothing)))
    ((syntax:false) (%gen-const #f))
    ((syntax:keyword) (%gen-const (%syntax-value expr)))
    ((syntax:list) (%gen-list (%syntax-value expr) env))
    ((syntax:method) (%gen-method (%syntax-value expr) env))
    ((syntax:nothing) (%gen-const (%nothing)))
    ((syntax:bignum) (%gen-const (%syntax-value expr)))
    ((syntax:fixnum) (%gen-const (%syntax-value expr)))
    ((syntax:flonum) (%gen-const (%syntax-value expr)))
    ((syntax:ratnum) (%gen-const (%syntax-value expr)))
    ((syntax:self-evaluating) (%gen-const (%syntax-value expr)))
    ((syntax:series) (%gen-series (%syntax-value expr) env))
    ((syntax:symbol) (%gen-variable-reference (%syntax-value expr) env))
    ((syntax:table) (%gen-table (%syntax-value expr) env))
    ((syntax:text) (%gen-const (%syntax-value expr)))
    ((syntax:true) (%gen-const #t))
    ((syntax:undefined) (%gen-const #!unbound))
    (else (error (str "Unrecognized syntax: " expr)))))

;;; (define $env (%add-frame (%null-env) (%make-frame (list (%make-lvar 'x 0 #f)(%make-lvar 'y 1 #t)))))
;;; (%gen (%expand-syntax (bard:read-from-string "#\\C")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "false")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "Foo:")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "nothing")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "[0 1 2 3]")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "99999999999999999999999999999999999999999999999999999999999")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "999")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "9.99")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "1.3e+12")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "2/3")) '())
;;; (%gen (%expand-syntax (bard:read-from-string "x")) $env)
;;; (%gen (%expand-syntax (bard:read-from-string "y")) $env)
;;; (%gen (%expand-syntax (bard:read-from-string "z")) $env)
;;; (%gen (%expand-syntax (bard:read-from-string "(^ [x] (* x x))")) $env)

;;; ---------------------------------------------------------------------
;;; 4. perform local optimizations
;;; ---------------------------------------------------------------------

(define (%optimize-locally ir env)
  #f)

;;; ---------------------------------------------------------------------
;;; 5. generate relocatable bytecode
;;; ---------------------------------------------------------------------

(define (%generate-bytecode optimized-ir)
  #f)

;;; ---------------------------------------------------------------------
;;; 6. load-time linking for the VM
;;; ---------------------------------------------------------------------
;;; replace symbolic labels with actual offsets; replace symbolic
;;; vm ops with actual procedures

(define (%load bytecode vm)
  #f)

;;; ---------------------------------------------------------------------
;;; toplevel interface to the compiler
;;; ---------------------------------------------------------------------

(define (%compile expr env)
  (let* ((standardized-expr (%expand-syntax expr))
         (optimized-expr (%optimize-syntax standardized-expr))
         (ir (%gen optimized-expr env))
         (optimized-ir (%optimize-locally ir env)))
    (%generate-bytecode optimized-ir)))

;;; (%compile (bard:read-from-string "#\\C") '())
;;; (%compile (bard:read-from-string "#\\space") '())
;;; (%compile (bard:read-from-string "#\\u0041") '())
;;; (%compile (bard:read-from-string "false") '())
;;; (%compile (bard:read-from-string "true") '())
;;; (%compile (bard:read-from-string "Foo:") '())
;;; (%compile (bard:read-from-string "nothing") '())
;;; (%compile (bard:read-from-string "()") '())
;;; (%compile (bard:read-from-string "[]") '())
;;; (%compile (bard:read-from-string "{}") '())
;;; (%compile (bard:read-from-string "\"\"") '())
;;; (%compile (bard:read-from-string "[0 1 2 3]") '())
;;; (%compile (bard:read-from-string "(^ (x)(* x x))") '())
;;; (%compile (bard:read-from-string "99999999999999999999999999999999999999999999999999999999999") '())
;;; (%compile (bard:read-from-string "999") '())
;;; (%compile (bard:read-from-string "9.99") '())
;;; (%compile (bard:read-from-string "1.3e+12") '())
;;; (%compile (bard:read-from-string "2/3") '())
;;; (%compile (bard:read-from-string "foo") '())
;;; (%compile (bard:read-from-string "Bar") '())
;;; (%compile (bard:read-from-string "|Foo Bar|") '())
;;; (%compile (bard:read-from-string "{}") '())
;;; (%compile (bard:read-from-string "{a: 1 b: 2}") '())
;;; (%compile (bard:read-from-string "(~)") '())
;;; (%compile (bard:read-from-string "(~ x in: [1 2])") '())
;;; (%compile (bard:read-from-string "(~ x in: NATURAL where: (odd? x))") '())
;;; (%compile (bard:read-from-string "(~ with: [[x 0] [y 1]] yield: [x y] then: [y (+ y 1)])") '())
;;; (%compile (bard:read-from-string "\"Foo bar baz\"") '())
;;; (%compile (bard:read-from-string "undefined") '())
