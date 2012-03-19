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

(let ((_env #f))
  (set! toplevel-environment
        (lambda ()
          (if _env
              _env
              (begin
                (set! _env (bard:make-cell (make-default-environment)))
                _env)))))

(define (define-variable! var val)
  (bard:put-cell! (toplevel-environment)
                  (extend-environment (bard:get-cell (toplevel-environment)))))

(define (lookup-variable-value var env)
  (let* ((top-env (bard:get-cell (toplevel-environment)))
         (entry (find-entry env var)))
    (if entry
        (cdr entry)
        (let ((entry (find-entry top-env var)))
          (if entry
              (cdr entry)
              (undefined))))))

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

(define (bard:eval expr env)
  (cond
   ((self-evaluating? expr) exp)
   ((symbol? expr)(lookup-variable-value expr env))
   (else (error "unrecognized expression type" expr))))

;;; (bard:eval '+ (bard:get-cell (toplevel-environment)))


