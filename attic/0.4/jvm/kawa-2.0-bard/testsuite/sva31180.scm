;; Savannah bug #31180: exception in inline-compiler
;; Note this only failed in immediate mode, with foo exported but bar private.
(module-export foo foo2)
(module-static #t)
(define (bar (port-number <int>) name)
  (list port-number name))

(define (foo port name) (bar port name))

;; Savannah bug 31256: Verify error
(define (foo2) ((bar2)))
(define (bar2) (lambda () 100))

;;Output: (5 foo)
;;Output: 100

