;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       VM lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; an environment is a stack of frames, in which the top of the
;;; stack is the current local environment, and each succeeding
;;; frame is an enclosing environment.
;;; 
;;; there are a couple of possible representations and some tradeoffs.
;;; if a stack of frames is represented as a vector of vectors, then
;;; read and write access to arbitrary variables is optimally fast.
;;; on the other hand, it means that when we make a closure, we have
;;; to shallow-copy the whole stack of frames, so that the closure
;;; continues to see its whole environment correctly.
;;;
;;; representing the environment stack as a list of frames means we
;;; don't have to copy the whole stack; instead, a closure's
;;; environment is just a single cons cell whose CAR is the local
;;; scope and whose CDR refers to the enclosing stack of frames.  that
;;; whole enclosing stack is reused for each lexical scope.
;;;
;;; for now we go with lists of vectors for the sake of the simplicity
;;; and avoiding the need to copy whole environment stacks.

(define (null-env) '())

(define (make-frame vals)
  (list->vector vals))

(define (push-frame frame env)(cons frame env))

(define (pop-frame env)(values (car env)(cdr env)))

(define (env-ref env i j)
  (vector-ref (list-ref env i) j))

(define (env-set! env i j val)
  (vector-set! (list-ref env i) j val))


