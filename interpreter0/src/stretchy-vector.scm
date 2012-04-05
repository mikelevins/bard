;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stretchy-vector.scm
;;;; Project:       Bard
;;;; Purpose:       expandable vectors
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")

;;; ---------------------------------------------------------------------
;;; stretchy vector
;;; ---------------------------------------------------------------------

(define (next-larger-power-of-two n)
  (let loop ((i 1))
    (let ((it (expt 2 i)))
      (if (> it n)
          it
          (loop (+ i 1))))))

(define-type stretchy-vector
  id: D2A81938-4CA6-430D-AE19-C7F5F4B6C89A
  constructor: %make-stretchy-vector
  (elements stretchy-vector-elements stretchy-vector-elements-set!)
  (fill-pointer stretchy-vector-fill-pointer stretchy-vector-fill-pointer-set!))

(define (make-stretchy-vector n #!optional (initial-element #f))
  (let ((elts (make-vector n initial-element)))
    (%make-stretchy-vector elts 0)))

(define (stretchy-vector-length vec)
  (stretchy-vector-fill-pointer vec))

(define (stretchy-vector-capacity vec)
  (vector-length (stretchy-vector-elements vec)))

(define (%truncate-stretchy-vector vec n)
  (let ((old-elts (stretchy-vector-elements vec))
        (new-elts (make-vector n #f)))
    (let loop ((i 0))
      (if (< i n)
          (begin
            (vector-set! new-elts i (vector-ref old-elts i))
            (loop (+ i 1)))))
    (stretchy-vector-elements-set! vec new-elts)
    vec))

(define (%expand-stretchy-vector vec n)
  (let* ((old-size (stretchy-vector-length vec))
         (new-size (next-larger-power-of-two n))
         (old-elts (stretchy-vector-elements vec))
         (new-elts (make-vector new-size #f)))
    (let loop ((i 0))
      (if (< i old-size)
          (begin
            (vector-set! new-elts i (vector-ref old-elts i))
            (loop (+ i 1)))))
    (stretchy-vector-elements-set! vec new-elts)
    vec))

(define (stretchy-vector-capacity-set! vec n)
  (let ((cap (stretchy-vector-capacity vec)))
    (cond
     ((= cap n) vec)
     ((< n cap)(%truncate-stretchy-vector vec n))
     (else (%expand-stretchy-vector vec n)))))

(define (stretchy-vector-ref vec n)
  (if (< n (stretchy-vector-length vec))
      (vector-ref (stretchy-vector-elements vec) n)
      (error "index out of range" n)))

(define (stretchy-vector-set! vec n val)
  (if (< n (stretchy-vector-length vec))
      (begin
        (vector-set! (stretchy-vector-elements vec) n val)
        vec)
      (if (< (+ n 1) (stretchy-vector-capacity vec))
          (begin
            (vector-set! (stretchy-vector-elements vec) n val)
            (stretchy-vector-fill-pointer-set! vec (max (+ n 1)(stretchy-vector-length vec)))
            vec)
          (begin
            (stretchy-vector-capacity-set! vec n)
            (stretchy-vector-set! vec n val)
            vec))))

