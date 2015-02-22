;; Some tests for abuse of keywords

(define f1 'key1:)
(define f2 key1:)
;; Diagnostic: bad-keywords1.scm:4:12: warning - keyword should be quoted if not in argument position

(list 0 k1: 3 k2: 5 8)

(list 0 k1: 3 4 k2: 5 8)
;; Diagnostic: bad-keywords1.scm:9:17: warning - keyword separated from other keyword arguments

(list 0 k1: 'k2: 5 8)
(list 0 k1: k2: 5 8)
;; Diagnostic: bad-keywords1.scm:13:13: warning - missing keyword value

(list 0 k1: 3 'k2:)
(list 0 k1: 3 k2:)
;; Diagnostic: bad-keywords1.scm:17:15: warning - missing keyword value

(list k1: k2: 5 9)
;; Diagnostic: bad-keywords1.scm:20:11: warning - missing keyword value
