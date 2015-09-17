(eval '(define (foo) 'a))
(eval '(define x foo))
(eval '(let ((x (lambda () 'b)))
         (format #t "~w; ~w; ~w~%" (x) x (foo))))
;; Output: b; #<procedure x>; a

;; This simplified/hacked version of next-leaf-generator from test.scm
;; used to cause a VerifyError (due to a bug in setCallersNeedStaticLink
;; in LambdaExp.java).
(define (next-leaf-generator1 eot)
  (letrec ((cont (lambda (x)
                   (cond (x (list x eot))
                         (else
                          (set! cont
                                (lambda (x) eot))
                          (cont #t))))))
    (cont #f)))
(format #t "lg1: ~a~%" (next-leaf-generator1 'eot))
;; Output: lg1: eot

(define (next-leaf-generator2 eot)
  (letrec ((cont (lambda (x)
                   (set! cont
                         (lambda (x) eot))
                   (cont #t))))
    (cont #f)))
(format #t "lg2: ~a~%" (next-leaf-generator2 'eot))
;; Output: lg2: eot
