;; This should return #t. If the bug present, code will not
;; compile. Test case for Savannah bug #39229
(format #t "~s~%" (equal? '|| (string->symbol "")))
;; Output: #t

;; Some tests of round-tripping symbols with colon and no namespace,
;; including testing of eval.

(format #t "short-type.class: ~s~%"
        (gnu.bytecode.Type:lookupType "short"))
;; Output: short-type.class: Type short
(format #t "eval.short-type.class: ~s~%"
        (eval '(gnu.bytecode.Type:lookupType "short")))
;; Output: eval.short-type.class: Type short

(format #t "six:~a~%"
        (let ((pr:x 13) (pr:y 7)) (- pr:x pr:y)))
;; Output: six:6
(format #t "seven:~a~%"
        (eval '(let ((pr:x 13) (pr:y 6)) (- pr:x pr:y))))
;; Output: seven:7

