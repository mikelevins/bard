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
  (values stack-values))

(define (make-stack size)
  (%make-stack 0 (make-vector size #f)))

(define (stack-size stack)
  (vector-length (stack-values stack)))

(define (push! stack v)
  (vector-set! (stack-values stack) (stack-count stack) v)
  (set-stack-count! stack (+ 1 (stack-count stack)))
  stack)

(define (pushn! stack vals)
  (let* ((scount (stack-count stack))
         (vcount (length vals))
         (limit (+ scount vcount)))
    (let loop ((i scount)
               (vs vals))
      (if (< i limit)
          (begin (vector-set! (stack-values stack) i (car vs))
                 (loop (+ i 1)
                       (cdr vs)))
          (set-stack-count! stack limit)))
    stack))

(define (pop! stack)
  (set-stack-count! stack (- (stack-count stack) 1))
  (vector-ref (stack-values stack) (stack-count stack)))

(define (popn! stack n)
  (let* ((scount (stack-count stack))
         (index (- scount 1))
         (svals (stack-values stack)))
    (let loop ((vals '())
               (count 0))
      (if (< count n)
          (loop (cons (vector-ref svals (- index count)) vals)
                (+ count 1))
          (begin
            (set-stack-count! stack (- scount count))
            vals)))))
