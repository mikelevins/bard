;; Java-options: "-Xmx20m"
(define-variable n 1)
(let ((lambda '(begin (define-variable n)
                      ((lambda ()
                         (set! n (+ n 2)))))))
;		 (+ x 42))))
  (let loop ((i 0))
    #|cd ..

    (when (= (mod i 1000) 0)
      (format #t "~d: ~:d\n" i ((java.lang.Runtime:getRuntime):totalMemory))
      (force-output))
    |#
    (eval lambda)
    (if (< i 10000)
    ;(if (< i 50)
        (loop (+ i 1)))))

(format #t "Done ~d.~%" n)
;; Output: Done 20003.
