

(define-macro (%method parms . body)
  `(%make-method name: #f
                 environment: '()
                 params: ',parms
                 body: '(begin ,@body)))

(define (%method-parms->lambda-list parms)
  (remove '& parms eq?))

(define (%method-parms->inline-parms parms)
  (remove '& parms eq?))

(define-macro (%primitive-method parms . body)
  (let ((fn (##gensym))
        (formal-parms (%method-parms->lambda-list parms))
        (inline-parms (%method-parms->inline-parms parms)))
    `(let ((,fn (lambda ,formal-parms ,@body)))
       (%make-method name: #f
                     environment: '()
                     params: ',parms
                     body: (cons ,fn ',inline-parms)))))


