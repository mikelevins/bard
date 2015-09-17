#!/usr/bin/env kawa
;; Kawa-options: %F "a b" c
(format #t "~w~%" (command-line))
;; The reason for the '[^ ]*' in the pattern is to make sur we
;; only have the fielname in (car (command-line)) - because of the #! above.
;; Assumed filename (and specifically $srcdir) does not contain spaces.
;; Output-pattern: [(]"[^ ]*command3.scm" "a b" "c"[)]
