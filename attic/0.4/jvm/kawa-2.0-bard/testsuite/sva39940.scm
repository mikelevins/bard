;; Savannah bug #39940
;; "Class member of type <procedure> compilation exception"

(define-simple-class <Simple1> (<Object>)
  (callback type: <procedure>)
  ((*init*)  (set! callback (lambda _ (format #t "Working1~%")))))
((make <Simple1>):callback)
;; Output: Working1

;; Same, but with a private field.
(define-simple-class <Simple1p> (<Object>)
  (callback type: <procedure> access: 'private)
  ((*init*)  (set! callback (lambda _ (format #t "Working1p~%"))))
  ((docallback) (callback)))
((make <Simple1p>):docallback)
;; Output: Working1p

(define-simple-class <Simple2> (<Object>)
  (callback type: <procedure>
            init: (lambda _ (format #t "Working2~%")))
  ((docallback) (callback)))
((make <Simple2>):docallback)
;; Output: Working2

;; Same, but with a private field.
(define-simple-class <Simple2p> (<Object>)
  (callback type: <procedure>  access: 'private
            init: (lambda _ (format #t "Working2p~%")))
  ((docallback) (callback)))
((make <Simple2p>):docallback)
;; Output: Working2p

(define (foo)
  (let ((f (lambda _ (format #t "Working3~%"))))
    (list f f)))
(format #t "~a~%" (car (foo)) ((cadr (foo))))
;; Output: Working3
;; Output: #<procedure f>
