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

(define-type adjustable-vector
  extender: def-adjustable-vector
  constructor: %make-adjustable-vector
  elements
  fill-pointer)

(define $adjustable-vector-segment-size 16)

(define (%compute-adjusted-vector-size length)
  (* $adjustable-vector-segment-size
     (+ (quotient length 16) 1) ))

(define (make-adjustable-vector)
  (let ((elts (make-vector $adjustable-vector-segment-size #!unbound)))
    (%make-adjustable-vector elts 0)))

(define (adjustable-vector-size vec)
  (vector-length (adjustable-vector-elements vec)))

(define (adjustable-vector-ref vec index)
  (vector-ref (adjustable-vector-elements vec) index))

(define (%expand-vector! vec #!optional new-cap)
  (let ((new-cap (if new-cap 
                     (%compute-adjusted-vector-size new-cap)
                     (%compute-adjusted-vector-size
                      (* 2 (adjustable-vector-fill-pointer vec)))))
        (old-cap (adjustable-vector-size vec)))
    (display new-cap)
    (if (> new-cap old-cap)
        (let ((new-elts (make-vector new-cap #!unbound)))
          (subvector-move! (adjustable-vector-elements vec) 0
                           (adjustable-vector-fill-pointer vec)
                           new-elts 0)
          (adjustable-vector-elements-set! vec new-elts)
          vec)
        vec)))

(define (%contract-vector! vec #!optional new-cap)
  (let ((new-cap (if new-cap 
                     (%compute-adjusted-vector-size new-cap)
                     (%compute-adjusted-vector-size
                      (adjustable-vector-fill-pointer vec))))
        (old-cap (adjustable-vector-size vec)))
    (if (< new-cap old-cap)
        (begin
          (vector-shrink! (adjustable-vector-elements vec)
                          new-cap))
        vec)))

(define (%adjust-vector! vec cap)
  (let ((old-cap (adjustable-vector-size vec)))
    (cond
     ((= cap old-cap) vec)
     ((< cap old-cap)(%contract-vector! vec cap))
     ((> cap old-cap)(%expand-vector! vec cap)))))

(define (%inc-fill-pointer! vec)
  (adjustable-vector-fill-pointer-set! vec (+ (adjustable-vector-fill-pointer vec) 1)))

(define (%dec-fill-pointer! vec)
  (adjustable-vector-fill-pointer-set! vec (- (adjustable-vector-fill-pointer vec) 1)))

(define (adjustable-vector-set! vec index val)
  (let ((old-fill (adjustable-vector-fill-pointer vec)))
    (cond
     ((< index 0)(error "out of range: " index))
     ((< index old-fill)(vector-set! (adjustable-vector-elements vec) index val))
     ((= index old-fill)(begin (vector-set! (adjustable-vector-elements vec) index val)
                               (%inc-fill-pointer! vec)))
     (else (error "out of range: " index)))
    vec))

(define (adjustable-vector-push! val vec)
  (with-exception-catcher
   (lambda (err)
     (if (range-exception? err)
         (begin
           (%expand-vector! vec (* 2 (adjustable-vector-fill-pointer vec)))
           (if (> (adjustable-vector-size vec)
                  (adjustable-vector-fill-pointer vec))
               (adjustable-vector-set! vec (adjustable-vector-fill-pointer vec) val)
               (raise err)))
         (raise err)))
   (lambda ()
     (adjustable-vector-set! vec (adjustable-vector-fill-pointer vec) val))))

(define (adjustable-vector-pop! vec)
  (let* ((fill (adjustable-vector-fill-pointer vec))
         (val (vector-ref (adjustable-vector-elements vec) 
                          (- fill 1))))
    (%dec-fill-pointer! vec)
    val))

(def-adjustable-vector stack
  constructor: %make-stack)

(define (make-stack)
  (let ((elts (make-vector $adjustable-vector-segment-size #!unbound)))
    (%make-stack elts 0)))

(define (push! v stack)
  (adjustable-vector-push! v stack))

(define (pushn! vals stack)
  (with-exception-catcher
   (lambda (err)
     (if (range-exception? err)
         (let* ((count (length vals)))
           (%adjust-vector! stack count)
           (if (> (adjustable-vector-size stack))
               (let* ((elts (list->vector vals)))
                 (subvector-move! elts 0 count
                                  (adjustable-vector-elements stack)
                                  (adjustable-vector-fill-pointer stack))
                 (adjustable-vector-fill-pointer-set! stack (+ (adjustable-vector-fill-pointer stack) count))
                 stack)))
         (raise err)))
   (lambda ()
     (let* ((elts (list->vector vals))
            (count (vector-length elts)))
       (subvector-move! elts 0 count
                        (adjustable-vector-elements stack)
                        (adjustable-vector-fill-pointer stack))
       (adjustable-vector-fill-pointer-set! stack (+ (adjustable-vector-fill-pointer stack) count))
       stack))))

(define (pop! stack)
  (adjustable-vector-pop! stack))

(define (popn! n stack)
  (let ((vals (vector->list (subvector (adjustable-vector-elements stack) 0 n))))
    (adjustable-vector-fill-pointer-set! stack (- (adjustable-vector-fill-pointer stack) n))
    vals))

(define (top stack)
  (vector-ref (adjustable-vector-elements stack)
              (- (adjustable-vector-fill-pointer stack) 1)))

