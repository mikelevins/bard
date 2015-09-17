;; Java-options: -Dkawa.command.name=command1
;; Kawa-options: %F a "b c" de
(format #t "~w~%" (command-line))
;; Output: ("command1" "a" "b c" "de")
