;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stack.scm
;;;; Project:       Bard
;;;; Purpose:       VM value stack
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type stack
  id: 99CC6B45-1C30-46A9-ACDC-D4B8519FB38A
  constructor: %make-stack
  elements pointer)

(define $default-stack-depth 256)

(define (make-stack #!optional (depth $default-stack-depth))
  (let ((elts (make-vector depth #f)))
    (%make-stack elts 0)))

(define (push! v stack)
  (let ((ptr (stack-pointer stack)))
    (vector-set! (stack-elements stack) ptr v)
    (stack-pointer-set! stack (+ ptr 1))
    stack))

(define (npush! vals stack)
  (let* ((elts (list->vector vals))
         (eltcount (vector-length elts))
         (ptr (stack-pointer stack)))
    (subvector-move! elts 0 eltcount (stack-elements stack) ptr)
    (stack-pointer-set! stack (+ ptr eltcount))
    stack))

(define (pop! stack)
  (let* ((ptr (- (stack-pointer stack) 1)))
    (stack-pointer-set! stack ptr)
    (vector-ref (stack-elements stack) ptr)))

(define (top stack)
  (vector-ref (stack-elements stack) 
              (- (stack-pointer stack) 1)))

(define (next stack)
  (vector-ref (stack-elements stack) 
              (- (stack-pointer stack) 2)))

(define (npop! n stack)
  (let* ((ptr (stack-pointer stack))
         (elts (subvector (stack-elements stack) (- ptr n) ptr)))
    (stack-pointer-set! stack (- ptr n))
    (vector->list elts)))

(define (depth stack)(stack-pointer stack))

