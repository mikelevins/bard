(define (reader-test)
  (let ((val-strings (list "'" "," "undefined" "nothing" "true"
                           "false" "1" "1.23" "1/2" "\\c" "\\space"
                           "\\u+0020" "\\u+0061" "Foo" "bard.core:Foo"
                           "\"Foo, bar!\"" "(+ 2 3)" "[+ 2 3]" "{+ 2 - 3}")))
    (newline)
    (for-each (lambda (s)
                (newline)
                (display s)
                (display " -> ")
                (display (bard:format-object (call-with-input-string s (lambda (in)(bard:read in))))))
              val-strings)
    (newline)))

;;; (reader-test)