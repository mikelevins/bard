(define (vector-position-if pred vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (let ((elt (vector-ref vec i)))
            (if (pred elt)
                i
                (loop (+ 1 i))))
          #f))))
