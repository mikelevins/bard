

(define-macro (%method parms . body)
  `(%make-method name: #f
                 environment: '()
                 params: ',parms
                 body: '(begin ,@body)))

(define-macro (%primitive-method parms . body)
  (letrec ((rm (lambda (item ls test acc)
                 (if (null? ls)
                     (reverse acc)
                     (if (test item (car ls))
                         (rm item (cdr ls) test acc)
                         (rm item (cdr ls) test (cons (car ls) acc)))))))
    (let ((fn (##gensym))
          (formal-parms (rm '& parms eq? '()))
          (inline-parms (rm '& parms eq? '())))
      `(let ((,fn (lambda ,formal-parms ,@body)))
         (%make-method name: #f
                       environment: '()
                       params: ',parms
                       body: (cons ,fn ',inline-parms))))))


