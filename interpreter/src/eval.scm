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
    (if (null? exprs)
        val
        (loop (cdr exprs)
              (%eval (car exprs) env)))))

(define (%eval-function-application expr env)
  (let* ((op (%eval (car expr) env))
         (args (map (lambda (x)(%eval x env))
                    (cdr expr))))
    (%apply op args)))

(define (%eval-application expr env)
  (cond
   ((%special-form? expr)(%eval-special-form expr env))
   ((%macro-form? expr)(%eval-macro-form expr env))
   (else (%eval-function-application expr env))))


(define (%eval expr #!optional (env '()))
  (cond
   ((symbol? expr) (%eval-variable expr env))
   ((list? expr) (cond
                   ((null? expr) expr)
                   ((eq? 'with-exit (car expr))
                    (let* ((form (cdr expr))
                           (exit-var (car (car form)))
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



