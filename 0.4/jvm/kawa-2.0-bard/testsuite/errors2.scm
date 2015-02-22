(module-compile-options warn-as-error: #t)

;; Swapped operand order.
(format #t (instance? <java.lang.String> 1234))
;; Diagnostic: errors2.scm:4:42: not a type or class expression

(define (bad-throwable1)
  (primitive-throw java.lang.NullPointerException))
;; Diagnostic: errors2.scm:8:20: type class is incompatible with required type java.lang.Throwable


