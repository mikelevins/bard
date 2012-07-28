
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

(define (%compile-application expr env)
  (let ((op-expr (car expr)))
    (cond
     ((eq? 'begin op-expr) (%compile-begin (cdr expr) env))
     ((eq? 'define op-expr) `(DEF ,(cadr expr) ,(%compile1 (caddr expr) env)))
     ((eq? 'quote op-expr) `(QUOTE ,(cadr expr)))
     (else (let ((op (%compile1 op-expr env))
                 (args (map (lambda (e)(%compile1 e env))
                            (cdr expr))))
             `(APP ,op ,args))))))

;;; compile to symbolic intermediate code
(define (%compile1 expr env)
  (let ((code (cond
               ((%nothing? expr)(%compile-constant expr))
               ((%symbol? expr)(%compile-var-ref expr env))
               ((%list? expr)(%compile-application expr env))
               (else (%compile-constant expr)))))
    code))


;;; constants
;;; (%compile1 (%read-from-string "nothing") '())
;;; (%compile1 (%read-from-string "-1") '())
;;; (%compile1 (%read-from-string "0") '())
;;; (%compile1 (%read-from-string "1") '())
;;; (%compile1 (%read-from-string "2") '())
;;; (%compile1 (%read-from-string "12") '())

;;; variables
;;; (%compile1 (%read-from-string "y") '(((x . 0))))
;;; (%compile1 (%read-from-string "bard.user:y") '(((x . 0))))
;;; (%compile1 (%read-from-string "y") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(+ x y)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "'(+ x y)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(set! y 2)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(define x 2)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(begin)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(begin 1)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(begin 1 2)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(begin 1 2 3)") '(((x . 0)(y . 1))))
;;; (%compile1 (%read-from-string "(begin 1 2 3 4)") '(((x . 0)(y . 1))))


