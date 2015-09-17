(define (bind-return loop-id k body)
  (let ((return-id (datum->syntax loop-id 'return)))
    #`(call/cc
       (lambda (#,k)
         (let-syntax ((#,return-id
                       (syntax-rules ()
                         ((_ exp)
                          (call-with-values (lambda () exp) #,k)))))
           #,body)))))

(define (simple-loop form)
  (syntax-case form ()
    ((loop-id forms ...)
     (bind-return
      #'loop-id #'k
      #`(let loop ()
          (begin forms ...)
          (loop))))))

(define-syntax xloop simple-loop)

(display (if (eq? (xloop (return #t)) #t)
             'ok
             'failed))
(newline)
;; Output: ok
