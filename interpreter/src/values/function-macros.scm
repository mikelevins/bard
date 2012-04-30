

(define-macro (%method parms . body)
  `(%make-method name: #f
                 environment: '()
                 params: ',parms
                 body: '(begin ,@body)))

(define-macro (%primitive-method parms . body)
  (let ((fn (##gensym)))
    `(let ((,fn (lambda ,parms ,@body)))
       (%make-method name: #f
                     environment: '()
                     params: ',parms
                     body: (cons ,fn ',parms)))))


