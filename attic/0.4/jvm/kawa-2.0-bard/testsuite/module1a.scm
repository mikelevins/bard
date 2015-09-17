(module-name <module1a>)
(module-static #t)

(defmacro define-abc-func (name)
  `(define (,name) 'abc))

;; Test separate compilation of type alias of existing classs.
(define-alias jlString java.lang.String)
