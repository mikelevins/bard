
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
        `(LREF ,(car ref) ,(cdr ref))
        (receive (vname mname)(parse-symbol-name expr)
          `(MREF ,mname ,vname)))))

(define (%compile-begin expr env)
  (if (null? expr)
      '()
      (if (null? (cdr expr))
          (%compile (car expr) env)
          (cons 'BEGIN
                (map (lambda (e)(%compile e env))
                     expr)))))

(define (%compile-setter expr env)
  (let ((varexpr (cadr expr)))
    (if (list? varexpr)
        (let ((varname (car varexpr))
              (objexpr (cadr varexpr)))
          `(SLOTSETTER ,varname ,(%compile objexpr env)))
        (let ((ref (in-env? varexpr env)))
          (if ref
              `(LSETTER ,(car ref) ,(cdr ref))
              (receive (vname mname)(parse-symbol-name varexpr)
                       `(MSETTER ,mname ,vname)))))))

(define (%make-method-lambda-bindings args)
  (map (lambda (arg)(make-binding arg #!unbound))
       args))

(define (%compile-method expr env)
  (let* ((lambda-list (cadr expr))
         (method-bindings (%make-method-lambda-bindings lambda-list))
         (body (cddr expr)))
    `(METHOD ,lambda-list ,(%compile-begin body (add-frame env method-bindings)) ,env)))

(define (%compile-if expr env)
  (let ((len (length expr)))
    (cond
     ((= len 3)(let ((test (list-ref expr 1))
                     (then (list-ref expr 2)))
                 `(IF ,(%compile test env) ,(%compile then env))))
     ((= len 4)(let ((test (list-ref expr 1))
                     (then (list-ref expr 2))
                     (else (list-ref expr 3)))
                 `(IF ,(%compile test env) ,(%compile then env) ,(%compile else env))))
     (else (error "Malformed if expression")))))

(define (%compile-definition expr env)
  (let* ((varexpr (cadr expr))
         (vname (if (list? varexpr)
                    (car varexpr)
                    varexpr))
         (mutable? (if (list? varexpr)
                       (and (eq? (cadr varexpr)
                                 (%read-from-string ":mutable"))
                            #t)
                       #f))
         (valexpr (%compile (caddr expr) env)))
    `(DEF ,vname ,valexpr ,mutable?)))

(define (%compile-application expr env)
  (let ((op-expr (car expr)))
    (cond
     ((%macro-form? expr) (%compile (%macroexpand expr) env))
     ((eq? 'begin op-expr) (%compile-begin (cdr expr) env))
     ((eq? 'if op-expr) (%compile-if expr env))
     ((eq? 'define op-expr) (%compile-definition expr env))
     ((eq? 'quote op-expr) `(QUOTE ,(cadr expr)))
     ((eq? 'setter op-expr) (%compile-setter expr env))
     ((eq? 'method op-expr) (%compile-method expr env))
     (else (let ((op (%compile op-expr env))
                 (args (map (lambda (e)(%compile e env))
                            (cdr expr))))
             `(APP ,op ,args))))))

;;; input: bard expressions (s-expressions)
;;; output: symbolic intermediate code
(define (%compile expr env)
  (let ((code (cond
               ((%nothing? expr)(%compile-constant expr))
               ((%symbol? expr)(%compile-var-ref expr env))
               ((%list? expr)(%compile-application expr env))
               (else (%compile-constant expr)))))
    code))

;;; input: symbolic intermediate code (output of compile)
;;; output: linked code (symbols replaced with functions)
(define (%link expr)
  (cond
   ((null? expr) expr)
   ((list? expr)(let ((link (%linker (car expr))))
                  (if link
                      (link expr)
                      expr)))
   (else expr)))


(define (comptest str)
  (let ((env (extend-env (extend-env (null-env) 'y 1 #t)
                         'x 2)))
    (%compile (%read-from-string str) $env)))

;;; CONST
;;; (%link (comptest "nothing"))             -> (CONST ())
;;; (%link (comptest "-1"))                  -> (CONST -1)
;;;
;;; LREF
;;; (%link (comptest "x"))                   -> (LREF 1 0)
;;;
;;; MREF
;;; (%link (comptest "z"))                   -> (MREF #f z)
;;; (%link (comptest "user.test:z"))         -> (MREF user.test z)
;;;
;;; BEGIN
;;; (%link (comptest "(begin 1 2)"))         -> (BEGIN (CONST 1) (CONST 2))
;;; (%link (comptest "(begin 1 2 3)"))       -> (BEGIN (CONST 1) (CONST 2) (CONST 3))
;;;
;;; SLOTSETTER
;;; (%link (comptest "(setter (name foo))")) -> (SLOTSETTER name (MREF #f foo))
;;; (comptest "(set! (name foo) 'bar)")      -> (APP (SLOTSETTER name (MREF #f foo)) ((QUOTE bar)))
;;;
;;; LSETTER
;;; (%link (comptest "(setter x)"))          -> (LSETTER 1 0)
;;;
;;; MSETTER
;;; (%link (comptest "(setter z)"))          -> (MSETTER #f z)
;;; (%link (comptest "(setter u.test:z)"))   -> (MSETTER u.test z)
;;;
;;; METHOD
;;; (%link (comptest "(method (a)(+ 1 a) 'ok)"))
;;;                                          -> (METHOD (a)
;;;                                          ->         (BEGIN (APP (MREF #f +) ((CONST 1) (LREF 0 0))) (QUOTE ok))
;;;                                          ->         (((y 2 . #<procedure #16>)) ((x 1 . #f))))
;;;
;;; DEF
;;; (%link (comptest "(define a 1)"))        -> (DEF a (CONST 1) #f)
;;; (%link (comptest "(define (a :mutable) 1)"))
;;;                                          -> (DEF a (CONST 1) #t)
;;;
;;; QUOTE
;;; (%link (comptest "'x"))                  -> (QUOTE x)
;;;
;;; APP
;;; (%link (comptest "(foo 2 3)"))           -> (APP (MREF #f foo) ((CONST 2) (CONST 3)))




