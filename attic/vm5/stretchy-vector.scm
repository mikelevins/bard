;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          stretchy-vector.scm
;;;; Project:       Bard
;;;; Purpose:       vectors that grow as-needed
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type stretchy-vector
  constructor: %private-make-stretchy-vector
  read-only:
  extend-count
  read-write:
  fill-pointer
  elements)

(define (make-stretchy-vector count #!optional (initial-element #f)(fill-pointer #f)(extend-count #f))
  (let* ((fill-pointer (or fill-pointer 0))
         (v (make-vector count initial-element)))
    (%private-make-stretchy-vector extend-count fill-pointer v)))

(define (stretchy-vector-length sv)(stretchy-vector-fill-pointer sv))
(define (stretchy-vector-capacity sv)(vector-length (stretchy-vector-elements sv)))

(define (stretchy-vector-ref sv n)
  (if (< n (stretchy-vector-length sv))
      (vector-ref (stretchy-vector-elements sv) n)
      (error "index out of range")))

(define (stretchy-vector-set! sv index val)
  (let ((len (stretchy-vector-length sv)))
    (if (> index len)
        (error "index out of range")
        (vector-set! (stretchy-vector-elements sv) index val))))

(define (vector-push-extend! sv val)
  (let ((len (stretchy-vector-length sv))
        (cap (stretchy-vector-capacity sv)))
    (if (= len cap)
        (let ((oldelts (stretchy-vector-elements sv))
              (newelts (make-vector (or (stretchy-vector-extend-count sv) (* 2 cap))
                                    #f)))
          (let ((newlen (vector-length newelts)))
            (let loop ((i 0))
              (if (>= i len)
                  (begin
                    (stretchy-vector-elements-set! sv newelts)
                    (stretchy-vector-set! sv len val)
                    (stretchy-vector-fill-pointer-set! sv (+ len 1)))
                  (begin
                    (vector-set! newelts i (vector-ref oldelts i))
                    (loop (+ i 1)))))))
        (begin
          (stretchy-vector-set! sv len val)
          (stretchy-vector-fill-pointer-set! sv (+ len 1))))))

(define (vector-pop! sv)
  (let* ((newlen (- (stretchy-vector-length sv) 1))
         (val (vector-ref (stretchy-vector-elements sv)
                          newlen)))
    (stretchy-vector-fill-pointer-set! sv newlen)
    val))
