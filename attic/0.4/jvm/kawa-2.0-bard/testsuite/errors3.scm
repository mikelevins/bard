(define b::boolean #t)
(try-catch
 (let ((i::int b))
   (format #t "Missing exception i: ~w~%" i))
 (ex java.lang.Exception
      (format #t "Caught: ~w~%" ex)))
;; Diagnostic: errors3.scm:2:1: warning - type boolean is incompatible with required type int
;; Output: Caught: java.lang.ClassCastException: java.lang.Boolean cannot be cast to java.lang.Integer
