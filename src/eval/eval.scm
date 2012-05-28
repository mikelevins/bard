;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          eval.scm
;;;; Project:       Bard
;;;; Purpose:       the evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%eval-variable var env)
  (if (eq? var 'undefined)
      (%undefined)
      (let ((val (%lookup-variable-value env var)))
        (if (%defined? val)
            val
            (let ((global-val (%global-value var)))
              (if (%defined? global-val)
                  global-val
                  (error (string-append "Undefined variable: " (object->string var)))))))))

(define (%eval-function-application expr env)
  (let ((expr (%map (lambda (x)(%eval x env)) expr)))
    (%apply (%car expr)(%cdr expr))))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))

(define (%eval expr #!optional (env '()))
  (cond
   ((%symbol? expr) (%eval-variable expr env))
   ((%list? expr) (if (%null? expr)
                      expr
                      (%eval-application expr env)))
   (else expr)))

;;; (%init-bard)
;;; (show (%eval (bard:read-from-string "undefined")))
;;; (show (%eval (bard:read-from-string "nothing")))
;;; (show (%eval (bard:read-from-string "true")))
;;; (show (%eval (bard:read-from-string "false")))
;;; (show (%eval (bard:read-from-string "5")))
;;; (show (%eval (bard:read-from-string "5.4")))
;;; (show (%eval (bard:read-from-string "5/4")))
;;; (show (%eval (bard:read-from-string "888888888")))
;;; (show (%eval (bard:read-from-string "#\\C")))
;;; (show (%eval (bard:read-from-string "#\\space")))
;;; (show (%eval (bard:read-from-string "#\\u0041")))
;;; (show (%eval (bard:read-from-string "\"Fred and Barney\"")))
;;; (show (%eval (bard:read-from-string "Fred")))
;;; (%defglobal 'Fred "Yes, it's Fred")
;;; (show (%eval (bard:read-from-string "Fred")))
;;; (show (%eval (bard:read-from-string "name:")))
;;; (show (%eval (bard:read-from-string "(0 1 2 3)")))
;;; (show (%eval (bard:read-from-string "[0 1 2 3]")))
;;; (show (%eval (bard:read-from-string "{0 1 2 3}")))

;;; (show (%eval (bard:read-from-string "(and)")))
;;; (show (%eval (bard:read-from-string "(and true true)")))
;;; (show (%eval (bard:read-from-string "(and false true)")))

;;; (show (%eval (bard:read-from-string "(begin)")))
;;; (show (%eval (bard:read-from-string "(begin 1 2)")))
;;; (show (%eval (bard:read-from-string "(begin 1 2 (and true false))")))


;;; (show (%eval (bard:read-from-string "(define x \"X\")")))
;;; (show (%eval (bard:read-from-string "x")))

;;; (show (%eval (bard:read-from-string "(if true 'yes)")))
;;; (show (%eval (bard:read-from-string "(if false 'yes)")))
;;; (show (%eval (bard:read-from-string "(if true 'yes 'no)")))
;;; (show (%eval (bard:read-from-string "(if false 'yes 'no)")))

;;; (show (%eval (bard:read-from-string "(let ())")))
;;; (show (%eval (bard:read-from-string "(let () 5)")))
;;; (show (%eval (bard:read-from-string "(let ((x 4)) x)")))
;;; (show (%eval (bard:read-from-string "(let ((x 4)(y 5)) y)")))

;;; (show (%eval (bard:read-from-string "(not true)")))
;;; (show (%eval (bard:read-from-string "(not false)")))
;;; (show (%eval (bard:read-from-string "(not 1)")))
;;; (show (%eval (bard:read-from-string "(not (not 1))")))

;;; (show (%eval (bard:read-from-string "(or)")))
;;; (show (%eval (bard:read-from-string "(or true)")))
;;; (show (%eval (bard:read-from-string "(or true false)")))
;;; (show (%eval (bard:read-from-string "(or false true false)")))
;;; (show (%eval (bard:read-from-string "(or false false false)")))

;;; (show (%eval (bard:read-from-string "'x")))
;;; (show (%eval (bard:read-from-string "(quote x)")))
;;; (show (%eval (bard:read-from-string "(quote x y)")))

;;; (show (%eval (bard:read-from-string "(set! a 'A)")))
;;; (show (%eval (bard:read-from-string "(define a 'A)")))
;;; (show (%eval (bard:read-from-string "a")))
;;; (show (%eval (bard:read-from-string "(set! a 'Hey!)")))
;;; (show (%eval (bard:read-from-string "a")))

;;; (show (%eval (bard:read-from-string "(time undefined)")))
;;; (show (%eval (bard:read-from-string "(time 1.2)")))
;;; (show (%eval (bard:read-from-string "(time \"Fred and Barney\")")))
;;; (show (%eval (bard:read-from-string "(time (and true true))")))
