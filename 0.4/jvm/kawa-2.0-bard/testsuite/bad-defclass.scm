(define-class)
(list (define-class))
(define-class name)
;; Diagnostic: bad-defclass.scm:1:1: missing class name
;; Diagnostic: bad-defclass.scm:3:1: missing class members
; This one is out of order, because it is reported at rewrite-time.
;; Diagnostic: bad-defclass.scm:2:7: define-class can only be used in <body>
