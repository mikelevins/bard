;; Test for object form in syntax expansion.
(define-syntax-case
  toString-er ()
  ((_ text param eqtest)
   #`(let* ((ob
             (object ()
                     ((equals param :: java.lang.Object) ::boolean eqtest)
                     ((toString) ::java.lang.String text))))
       (format #f "~w ~w" (invoke ob 'toString) (invoke ob 'equals #t)))))

(format #t "~a~%" (toString-er "Hello!" x #f))
;; Output: "Hello!" #f
