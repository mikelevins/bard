;; Kawa-options: %F "a b" c
(format #t "~w~%" (command-line))
;; Output-pattern: [(]"java kawa.repl .*command2.scm" "a b" "c"[)]
