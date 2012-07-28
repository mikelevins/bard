
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%compile-constant expr) `(CONST ,expr))

(define (%compile-var-ref expr env) 
  (let ((ref (in-env? expr env)))
    (if ref
        `(LREF ,env ,(car ref) ,(cdr ref))
        (receive (vname mname)(parse-symbol-name expr)
          `(MREF ,mname ,vname)))))

(define (%compile-begin expr env)
  (if (null? expr)
      '()
      (if (null? (cdr expr))
          (%compile1 (car expr) env)
          (cons 'DROP1
           (list (%compile1 (car expr) env)
                 (%compile-begin (cdr expr) env))))))

(define (%compile-setter expr env)
  (let ((varexpr (cadr expr)))
    (if (list? varexpr)
        (let ((varname (car varexpr))
              (objexpr (cadr varexpr)))
          `(SLOTSETTER ,varname ,@(%compile1 objexpr env)))
        (let ((ref (in-env? varexpr env)))
          (if ref
              `(LSETTER ,ref env)
              `(MSETTER ,varexpr))))))

(define $macroexpanders (make-table test: eq?))

(table-set! $macroexpanders 'set!
            (lambda (expr) `((setter ,(cadr expr)) ,(caddr expr))))

(define (%macro-form? expr)
  (table-ref $macroexpanders (car expr) #f))

(define (%macroexpand expr)
  ((table-ref $macroexpanders (car expr) #f) expr))

(define $primitives (make-table test: eq?))

(table-set! $primitives 'PRIM+ 
            (make-primitive 'PRIM+ 2 (lambda (x y)(+ x y))))

(define (%primitive? expr)
  (table-ref $primitives expr #f))

(define (%make-method-lambda-bindings args)
  (map (lambda (arg)(make-binding arg #!unbound))
       args))

(define (%compile-method expr env)
  (let* ((lambda-list (cadr expr))
         (method-bindings (%make-method-lambda-bindings lambda-list))
         (body (cddr expr)))
    `(METHOD ,lambda-list ,method-bindings ,(%compile-begin body (add-frame env method-bindings)) ,env)))

(define (%compile-if expr env)
  (let ((len (length expr)))
    (cond
     ((= len 3)(let ((test (list-ref expr 1))
                     (then (list-ref expr 2)))
                 `(IF ,(%compile1 test env) ,(%compile1 then env))))
     ((= len 4)(let ((test (list-ref expr 1))
                     (then (list-ref expr 2))
                     (else (list-ref expr 3)))
                 `(IF ,(%compile1 test env) ,(%compile1 then env) ,(%compile1 else env))))
     (else (error "Malformed if expression")))))

(define (%compile-application expr env)
  (let ((op-expr (car expr)))
    (cond
     ((%macro-form? expr) (%compile1 (%macroexpand expr) env))
     ((eq? 'begin op-expr) (%compile-begin (cdr expr) env))
     ((eq? 'if op-expr) (%compile-if expr env))
     ((eq? 'define op-expr) `(DEF ,(cadr expr) ,(%compile1 (caddr expr) env)))
     ((eq? 'quote op-expr) `(QUOTE ,(cadr expr)))
     ((eq? 'setter op-expr) (%compile-setter expr env))
     ((eq? 'method op-expr) (%compile-method expr env))
     ((%primitive? op-expr) (let ((primexpr (car expr))
                                  (args (map (lambda (e)(%compile1 e env))
                                             (cdr expr))))
                              `(APPLYPRIM ,primexpr ,args)))
     (else (let ((op (%compile1 op-expr env))
                 (args (map (lambda (e)(%compile1 e env))
                            (cdr expr))))
             `(APP ,op ,args))))))

;;; output is symbolic intermediate code
(define (%compile1 expr env)
  (let ((code (cond
               ((%nothing? expr)(%compile-constant expr))
               ((%symbol? expr)(%compile-var-ref expr env))
               ((%list? expr)(%compile-application expr env))
               (else (%compile-constant expr)))))
    code))


;;; (define $env (extend-env (extend-env (null-env) 'x 1) 'y 2 #t))
;;; constant
;;; (%compile1 (%read-from-string "nothing") $env)
;;; (%compile1 (%read-from-string "-1") $env)
;;; symbol
;;; (%compile1 (%read-from-string "x") $env)
;;; (%compile1 (%read-from-string "y") $env)
;;; (%compile1 (%read-from-string "z") $env)
;;; list
;;; macro
;;; (%compile1 (%read-from-string "(set! x 5)") $env)
;;; (%compile1 (%read-from-string "(set! z 5)") $env)
;;; special forms
;;; (%compile1 (%read-from-string "(begin 1 2 3)") $env)
;;; (%compile1 (%read-from-string "(if true 1 0)") $env)
;;; (%compile1 (%read-from-string "(if false 0 1)") $env)
;;; (%compile1 (%read-from-string "(define foo 12)") $env)
;;; (%compile1 (%read-from-string "(quote foo)") $env)
;;; (%compile1 (%read-from-string "((setter foo) 5)") $env)
;;; (%compile1 (%read-from-string "(method (x)(+ x 1))") $env)
;;; (%compile1 (%read-from-string "(PRIM+ 2 3)") $env)
