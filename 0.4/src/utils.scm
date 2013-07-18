;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define (identity x) x)
(define (1+ n)(+ n 1))

(define (vector-for-each fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (let ((it (vector-ref vec i)))
            (fn it)
            (loop (+ i 1)))
          vec))))

(define (vector-map fn vec)
  (let* ((len (vector-length vec))
         (outvec (make-vector len #f)))
    (let loop ((i 0))
      (if (< i len)
          (let ((it (vector-ref vec i)))
            (vector-set! outvec i (fn it)))))
    outvec))
