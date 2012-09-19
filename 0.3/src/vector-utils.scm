;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vector-utils.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose vector utilities 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (vector-map fn . vecs)
  (if (null? vecs)
      (vector)
      (let* ((lens (map vector-length vecs))
             (len (apply min lens))
             (resultvec (make-vector len #f)))
        (let loop ((i 0))
          (if (< i len)
              (begin
                (vector-set! resultvec i (apply fn (map (lambda (v) (vector-ref v i)) 
                                                        vecs)))
                (loop (+ 1 i)))
              resultvec)))))

#| tests

(vector-map odd? (vector))
(vector-map odd? (vector 0))
(vector-map odd? (vector 0 1 2 3 4 5 6))
(vector-map + (vector 0 0 1 2 3 5)(vector 0 1 2 3 5 8))
(vector-map < (vector 0 1 2)(vector 1 2 3 4))

|#
