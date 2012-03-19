;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       bard interpreter in Scheme
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; gambit prerequisutes
;;;---------------------------------------------------------------------

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

;;;---------------------------------------------------------------------
;;; general utilities
;;;---------------------------------------------------------------------

(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

(define (->table . k/v-plist)
  (let ((tbl (make-table test: eq?)))
    (let loop ((kvs k/v-plist))
      (if (null? kvs)
          tbl
          (if (null? (cdr kvs))
              (error "odd number of inits in make-table" k/v-plist)
              (begin
                (table-set! tbl (car kvs)(cadr kvs))
                (loop (cddr kvs))))))))

;;;---------------------------------------------------------------------
;;; bard cells
;;;---------------------------------------------------------------------

(define (bard:make-cell value)
  (spawn 
   (lambda ()
     (let loop ((value value))
       (recv
        ((from tag 'ref)
         (! from (list tag value))
         (loop value))
        (('set! value)
         (loop value)))))))

(define (bard:get-cell cell)
  (!? cell 'ref))

(define (bard:put-cell! cell val)
  (! cell (list 'set! val)))

;;; (define $c (bard:make-cell #f))
;;; (bard:get-cell $c)
;;; (bard:put-cell! $c 1001)
;;; (bard:get-cell $c)

;;;---------------------------------------------------------------------
;;; bard values
;;;---------------------------------------------------------------------

(define (undefined) #!void)

;;;---------------------------------------------------------------------
;;; bard primitives
;;;---------------------------------------------------------------------

(define (prim:+ . args)(apply + args))
(define (prim:- . args)(apply - args))
(define (prim:* . args)(apply * args))
(define (prim:/ . args)(apply / args))

;;;---------------------------------------------------------------------
;;; environments
;;;---------------------------------------------------------------------

(define (null-environment) '())

(define (add-binding env var val)
  (cons (cons var val)
        env))

(define (extend-evironment env plist)
  (let loop ((env env)
             (bindings plist))
    (if (null? bindings)
        env
        (if (null? (cdr bindings))
            (error "malformed bindings list" plist)
            (let ((k (car bindings))
                  (v (cadr bindings))
                  (more (cddr bindings)))
              (loop (cons (cons k v)
                          env)
                    more))))))

(define (make-default-environment)
  (extend-environment (null-environment)
                      (list
                       '+ prim:+
                       '- prim:-)))

(define (find-entry env var)
  (assq var env))

(define toplevel-environment #f)
(define set-toplevel-environment! #f)

(let ((_env #f))
  (set! toplevel-environment
        (lambda ()
          (if _env
              (bard:get-cell _env)
              (begin
                (set! _env (bard:make-cell (make-default-environment)))
                (bard:get-cell _env)))))
  (set! set-toplevel-environment!
        (lambda (env)
          (if _env
              (bard:put-cell! _env env)
              (begin
                (set! _env (bard:make-cell env))
                (bard:get-cell _env))))))

(define (define-variable! var val)
  (set-toplevel-environment!
   (add-binding (toplevel-environment)
                var val)))

(define (lookup-variable-value var env)
  (let* ((top-env (toplevel-environment))
         (entry (find-entry env var)))
    (if entry
        (cdr entry)
        (let ((entry (find-entry top-env var)))
          (if entry
              (cdr entry)
              (undefined))))))

;;;---------------------------------------------------------------------
;;; special forms
;;;---------------------------------------------------------------------

(define (bard:define-variable var val)
  (define-variable! var val))

(define $special-forms-table
  (->table
   'define bard:define-variable))

(define (special-form? expr)
  (and (list? expr)
       (table-ref $special-forms-table (car expr))
       #t))

;;;---------------------------------------------------------------------
;;; macros
;;;---------------------------------------------------------------------

(define (macro? expr)
  #f)

;;;---------------------------------------------------------------------
;;; evaluator
;;;---------------------------------------------------------------------

(define (self-evaluating? x)
  (or (eq? x (undefined))
      (null? x)
      (boolean? x)
      (number? x)
      (string? x)
      (keyword? x)))

(define (bard:eval-special-form expr env)
  (let ((op (table-ref $special-forms-table (car expr))))
    (apply op (cdr expr))))

(define (bard:eval-macro expr env)
  #f)

(define (bard:apply op args)
  (apply op args))

(define (bard:eval-application expr env)
  (let ((op (bard:eval (car expr) env))
        (args (map (lambda (x)(bard:eval x env))
                   (cdr expr))))
    (bard:apply op args)))

(define (bard:eval-list expr env)
  (cond
   ((special-form? expr)(bard:eval-special-form expr env))
   ((macro? expr)(bard:eval-macro expr env))
   (else (bard:eval-application expr env))))

(define (bard:eval expr env)
  (cond
   ((self-evaluating? expr) exp)
   ((symbol? expr)(lookup-variable-value expr env))
   ((list? expr)(bard:eval-list expr env))
   (else (error "unrecognized expression type" expr))))

;;; (bard:eval '+ (toplevel-environment))
;;; (bard:eval '- (toplevel-environment))
;;; (bard:eval '(define x 3) (toplevel-environment))
;;; (bard:eval 'x (toplevel-environment))


