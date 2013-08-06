;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vmstate.scm
;;;; Project:       Bard
;;;; Purpose:       representation of the bard vm state (i.e. registers)
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; vmstate structure
;;; ----------------------------------------------------------------------

(define-structure vmstate program function pc nvals stack env globals haltfn)

;;; acessing and updating state

(define (vmstate-incpc! s)
  (vmstate-pc-set! s (1+ (vmstate-pc s))))

(define (vmstate-push! s v)
  (vmstate-stack-set! s (cons v (vmstate-stack s)))
  (vmstate-nvals-set! s (+ 1 (vmstate-nvals s))))

(define (vmstate-pushvals! s vals)
  (vmstate-stack-set! s (append vals (vmstate-stack s)))
  (vmstate-nvals-set! s (+ (length vals) (vmstate-nvals s))))

(define (vmstate-top s)
  (car (vmstate-stack s)))

(define (vmstate-pop! s)
  (let ((old-stack (vmstate-stack s)))
    (vmstate-stack-set! s (cdr old-stack))
    (vmstate-nvals-set! s (- (vmstate-nvals s) 1))
    (car old-stack)))

(define (vmstate-popn! s n)
  (let ((vals (take n (vmstate-stack s)))
        (stack* (drop n (vmstate-stack s))))
    (vmstate-stack-set! s stack*)
    (vmstate-nvals-set! s (- (vmstate-nvals s) n))
    vals))

(define (vmstate-gref s var)
  (table-ref (vmstate-globals s) var +absent+))

(define (vmstate-gset! s var val)
  (table-set! (vmstate-globals s) var val))

(define (vmstate-lref s var)
  (let ((entry (assq var (vmstate-env s))))
    (or entry +absent+)))

(define (vmstate-lset! s var val)
  (let ((entry (assq (vmstate-env s) var)))
    (if entry
        (begin
          (set-cdr! entry val)
          val)
        (let ((entry (cons var val)))
          (vmstate-env-set! s (cons entry (vmstate-env s)))
          val))))

(define (vmstate-apply! state)
  (let* ((fn (vmstate-pop! state))
         (args (vmstate-popn! state (vmstate-nvals state)))
         (call-env (make-fn-env fn args)))
    (vmstate-fn-set! state fn)
    (vmstate-code-set! (fn-code fn))
    (vmstate-pc-set! state 0)
    (vmstate-env-set! state call-env)))

(define (vmstate-return! state rr)
  (vmstate-fn-set! state (return-fn rr))
  (vmstate-code-set! state (fn-code (return-fn rr)))
  (vmstate-pc-set! state (return-pc rr))
  (vmstate-stack-set! state (return-stack rr))
  (vmstate-env-set! state (return-env rr))
  state)

(define (vmstate-setcc! state cc)
  (vmstate-stack-set! state (continuation-stack cc))
  state)

