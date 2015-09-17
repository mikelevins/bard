;; Kawa-options: --diagnostic-strip-directories --script %F
(define sb (java.lang.StringBuilder))
(sb:append "abcdefg")
(sb:setLength 3)
(display (sb:toString))(newline)
;; Diagnostic: reflect2.scm:3:2: warning - no known slot 'append' in java.lang.Object
;; Diagnostic: reflect2.scm:4:2: warning - no known slot 'setLength' in java.lang.Object
;; Output: abc
