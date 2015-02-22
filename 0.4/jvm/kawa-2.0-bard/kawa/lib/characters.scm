(require <kawa.lib.prim_syntax>)
(require <kawa.lib.compile_misc>)

(define (char? x)
  (or (instance? x gnu.text.Char) (instance? x java.lang.Character)))

(define (char->integer ch::character-or-eof) ::int
  validate-apply: "kawa.lib.compile_misc:charToIntegerValidateApply"
  (as int ch))

(define (integer->char (n <int>)) ::character
  validate-apply: "kawa.lib.compile_misc:integerToCharValidateApply"
  (as character n))

(define (digit-value ch::character)
  (let ((r (java.lang.Character:digit (char->integer ch) 10)))
    (if (< r 0) #f (->integer r))))
