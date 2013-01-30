;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stack.scm
;;;; Project:       Bard
;;;; Purpose:       finite value stacks
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type stack
  constructor: %make-stack
  (count stack-count set-stack-count!)
  (vals stack-values))

(define (make-stack size)
  (%make-stack 0 (make-vector size #f)))

(define (push! stack v)
  (let ((count (stack-count stack)))
    (vector-set! (stack-values stack) count v)
    (set-stack-count! stack (+ 1 count))
    stack))

(define (pop! stack)
  (let* ((new-count (- (stack-count stack) 1)))
    (set-stack-count! stack new-count)
    (vector-ref (stack-values stack) new-count)))

