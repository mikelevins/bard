;; Test run-time reflection.
;; Testcase from Bill Robinson <airbaggins@gmail.com>
(define-simple-class TestClass ()
  ((test (arg :: int)) :: void
   (java.lang.System:out:format "test %d\n" arg)))

(let ((t ::TestClass (TestClass))
      (method (string-copy "test"))) ;; avoid unintended inlining
  (invoke t 'test 1)
  (invoke t method 2))

;; Output: test 1
;; Output: test 2

