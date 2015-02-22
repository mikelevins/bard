;; Based on Savannah bug# 35728	"More unreachable code".
(define (foo)
  (call/cc
   (lambda (return)
     (let ((a 1))
       (let loop ((a a))
         (let ((finish (lambda (a) (return #f))))
           (finish a)
           (let ((a 2))
             (loop a))))))))
;; Diagnostic: unreach2.scm:9:12: warning - unreachable code
(format #t "foo: ~s~%" (foo))
;; Output: foo: #f
