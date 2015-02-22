(module-export find)

(define (find x list)
  (find-if list
           (lambda (y)
             (equal? x y))
           #f))

(define (find-if (i java.lang.Iterable) test default)
  (let ((iter (invoke i 'iterator))
        (found #f))
    (do ()
        ((or found (not (invoke iter 'has-next))))
      (let ((e (invoke iter 'next)))
        (when (test e)
          (set! found #t)
          (set! default e))))
    default))

(format #t "~s ~s~%" (find 3 #(8 7 13 2)) (find 13 #(8 7 13 2)))
;; Output: #f 13
