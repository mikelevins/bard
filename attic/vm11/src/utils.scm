

(define (str . args)
  (if (null? args)
      ""
      (let* ((arg (car args))
             (s (if (string? arg)
                    arg
                    (object->string arg))))
        (if (null? (cdr args))
            s
            (string-append s (apply str (cdr args)))))))
