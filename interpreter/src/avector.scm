;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          avector.scm
;;;; Project:       Bard
;;;; Purpose:       adjustable vectors
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type adjustable-vector
  id: 450A16E5-B4BF-4C81-A9A7-E1FACE726029
  constructor: %private-make-adjustable-vector
  (increment adjustable-vector-increment set-adjustable-vector-increment!)
  (default-element adjustable-vector-default-element set-adjustable-vector-default-element!)
  (slots adjustable-vector-slots set-adjustable-vector-slots!)
  (fill-pointer adjustable-vector-fill-pointer set-adjustable-vector-fill-pointer!))

(define (make-adjustable-vector initial-length #!key (increment 8)(default-element #f))
  (let* ((factor (+ 1 (truncate (/ initial-length increment))))
         (len (* factor increment))
         (vec (make-vector len #f)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (vector-set! vec i default-element)
            (loop (+ 1 i)))))
    (%private-make-adjustable-vector increment default-element vec 0)))

(define (expand-adjustable-vector! av)
  (let* ((old-cap (adjustable-vector-capacity av))
         (len (adjustable-vector-length av))
         (increment (adjustable-vector-increment av))
         (factor (+ 1 (truncate (/ len increment))))
         (elt (adjustable-vector-default-element av))
         (new-cap (* factor increment))
         (old-slots (adjustable-vector-slots av))
         (new-slots (make-vector new-cap elt)))
    (subvector-move! old-slots 0 len new-slots 0)
    (set-adjustable-vector-slots! av new-slots)
    av))

(define (adjustable-vector-capacity av)
  (vector-length (adjustable-vector-slots av)))

(define adjustable-vector-length adjustable-vector-fill-pointer)

(define (adjustable-vector-ref av i)
  (assert (< i (adjustable-vector-fill-pointer av)) (str "Index " i " is out of range"))
  (vector-ref (adjustable-vector-slots av) i))

(define (adjustable-vector-set! av i v)
  (assert (< i (adjustable-vector-fill-pointer av)) (str "Index " i " is out of range"))
  (vector-set! (adjustable-vector-slots av) i v))

(define (adjustable-vector-push! av v)
  (let ((cap (adjustable-vector-capacity av))
        (len (adjustable-vector-length av)))
    (if (>= len cap)
        (expand-adjustable-vector! av))
    (vector-set! (adjustable-vector-slots av) len v)
    (set-adjustable-vector-fill-pointer! av (+ len 1))
    av))

(define (adjustable-vector-position test av)
  (vector-position test (adjustable-vector-slots av) end: (adjustable-vector-length av)))
