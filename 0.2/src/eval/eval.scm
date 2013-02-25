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

(define (%eval-sequence seq env)
  (let loop ((exprs seq)
             (val (%nothing)))
    (if (%null? exprs)
        val
        (loop (cdr exprs)
              (%eval (%car exprs) env)))))

(define (%eval-function-application expr env)
  (let* ((op (%eval (%car expr) env))
         (args (map (lambda (x)(%eval x env))
                    (%cdr expr))))
    (%apply op args)))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))


(define (%eval expr #!optional (env '()))
  (cond
   ((%symbol? expr) (%eval-variable expr env))
   ((%list? expr) (cond
                   ((%null? expr) expr)
                   ((eq? 'with-exit (%car expr))
                    (let* ((form (%cdr expr))
                           (exit-var (%car (%car form)))
                           (body (%cons 'begin (%drop 1 form))))
                      (call/cc (lambda (k)(%eval body (%add-binding env exit-var k))))))
                   (else (%eval-application expr env))))
   (else expr)))


(define (%bard-load path)
  (newline)
  (display (string-append "Loading " path "..."))
  (newline)
  (call-with-input-file path
    (lambda (in)
      (let loop ((form (bard:read in)))
        (if (eqv? form #!eof)
            (newline)
            (begin
              (newline)
              (display (%as-string (%eval form (%null-environment))))
              (loop (bard:read in))))))))

(define (%bard-load-from-string load-string)
  (let ((error-handler (lambda (err)
                         (display (error->string err))
                         #f))
        (reader (lambda () 
                  (call-with-input-string 
                   load-string
                   (lambda (in)
                     (let loop ((form (bard:read in)))
                       (if (eqv? form #!eof)
                           (newline)
                           (begin
                             (%eval form (%null-environment))
                             (loop (bard:read in)))))))
                  #t)))
    (with-exception-catcher error-handler reader)))

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

;;; (show (%eval (bard:read-from-string "(function)")))
;;; (show (%eval (bard:read-from-string "(function foo)")))

;;; (show (%eval (bard:read-from-string "(method ())")))
;;; (show (%eval (bard:read-from-string "(method foo ())")))
;;; (show (%eval (bard:read-from-string "(method foo (x) x)")))
;;; (show (%eval (bard:read-from-string "(method foo (x) (begin (* x x)))")))

;;; (show (%eval (bard:read-from-string "(+ 2 3)")))


