;; Diagnostic: convert1.scm:12:60: warning - type character is incompatible with required type int
;; Diagnostic: convert1.scm:18:57: warning - type integer is incompatible with required type character
;; Diagnostic: convert1.scm:18:2: warning - cannot convert literal (of type gnu.math.IntNum) to ClassType gnu.text.Char

(format #t "i->c 123: ~w~%" (integer->char 123))
;; Output: i->c 123: #\{

(format #t "c->i }: ~w~%" (char->integer #\}))
;; Output: c->i }: 125

(try-catch
 (format #t "should-fail i->c #\x123: ~w~%" (integer->char #\x123))
 (ex java.lang.ClassCastException
     (format #t "caught from i->c 123: ~w~%" ex)))
;; Output: caught from i->c 123: java.lang.ClassCastException: gnu.text.Char cannot be cast to java.lang.Integer

(try-catch
 (format #t "should-fail c->i 120: ~w~%" (char->integer 120))
  (ex java.lang.ClassCastException
      (format #t "caught from c->i 120: ~w~%" ex)))
;; Output: caught from c->i 120: java.lang.ClassCastException: gnu.math.IntNum cannot be cast to gnu.text.Char
