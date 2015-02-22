;; "formatst.scm" SLIB FORMAT Version 3.0 conformance test
; Written by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
; Modified for Kawa by Per Bothner <per@bothner.com>.
;
; This code is in the public domain.

;; Test run: (slib:load "formatst")

; Failure reports for various scheme interpreters:
;
; SCM4d 
;   None.
; Elk 2.2:
;   None.
; MIT C-Scheme 7.1:
;   The empty list is always evaluated as a boolean and consequently
;   represented as `#f'.
; Scheme->C 01nov91:
;   None, if format:symbol-case-conv and format:iobj-case-conv are set
;   to string-downcase. 

;(require 'format)
;(if (not (string=? format:version "3.0"))
;    (begin
;      (display "You have format version ")
;      (display format:version)
;      (display ". This test is for format version 3.0!")
;      (newline)
;      (format:abort)))

(test-begin "format" 425)
(define-syntax test 
  (syntax-rules ()
    ((test format-args out-str)
     (test-equal out-str (apply format #f format-args)))))

(define slib:tab (integer->char 9))
(define slib:form-feed (integer->char 12))

(define format:symbol-case-conv #f)
(define format:floats #t)
(define format:complex-numbers #f)

(define format:iobj-case-conv #f)
;; As format:symbol-case-conv but applies for the representation of
;; implementation internal objects.

;; format:iobj->str reveals the implementation dependent representation of 
;; #<...> objects with the use of display and call-with-output-string.

(define format:read-proof #f)

(define (format:iobj->str iobj)
  (if format:iobj-case-conv
      (string-append 
       (if format:iobj-case-conv
	   (format:iobj-case-conv
	    (call-with-output-string (lambda (p) (display iobj p))))
	   (call-with-output-string (lambda (p) (display iobj p)))))
      (call-with-output-string (lambda (p) (display iobj p)))))

; ensure format default configuration

;(set! format:symbol-case-conv #f)
;(set! format:iobj-case-conv #f)

;; Slib specific: (format #t "~q")

;; Kawa does have complex numbers, but does not support the
;; non-standard ~i format specifier.
;;(format #t "This implementation has~@[ no~] flonums ~
;;            ~:[but no~;and~] complex numbers~%"
;;	(not format:floats) format:complex-numbers)

; any object test

(test '("abc") "abc")
(test '("~a" 10) "10")
(test '("~a" -1.2) "-1.2")
(test '("~a" a) "a")
(test '("~a" #t) "#t")
(test '("~a" #f) "#f")
(test '("~a" "abc") "abc")
;;SLIB version: (test '("~a" '#(1 2 3)) "#(1 2 3)")
(test '("~a" #(1 2 3)) "#(1 2 3)")
(test '("~a" ()) "()")
(test '("~a" (a)) "(a)")
(test '("~a" (a b)) "(a b)")
(test '("~a" (a (b c) d)) "(a (b c) d)")
(test '("~a" (a . b)) "(a . b)")
;;SLIB version: (test '("~a" (a (b c . d))) "(a (b . (c . d)))") ; this is ugly
(test '("~a" (a (b c . d))) "(a (b c . d))")
(test `("~a" ,display) (format:iobj->str display))
(test `("~a" ,(current-input-port)) (format:iobj->str (current-input-port)))
(test `("~a" ,(current-output-port)) (format:iobj->str (current-output-port)))

; # argument test

(test '("~a ~a" 10 20) "10 20")
(test '("~a abc ~a def" 10 20) "10 abc 20 def")

; numerical test

(test '("~d" 100) "100")
(test '("~x" 100) "64")
(test '("~o" 100) "144")
(test '("~b" 100) "1100100")
(test '("~@d" 100) "+100")
(test '("~@d" -100) "-100")
(test '("~@x" 100) "+64")
(test '("~@o" 100) "+144")
(test '("~@b" 100) "+1100100")
(test '("~10d" 100) "       100")
(test '("~:d" 123) "123")
(test '("~:d" 1234) "1,234")
(test '("~:d" 12345) "12,345")
(test '("~:d" 123456) "123,456")
(test '("~:d" 12345678) "12,345,678")
(test '("~:d" -123) "-123")
(test '("~:d" -1234) "-1,234")
(test '("~:d" -12345) "-12,345")
(test '("~:d" -123456) "-123,456")
(test '("~:d" -12345678) "-12,345,678")
(test '("~10:d" 1234) "     1,234")
(test '("~10:d" -1234) "    -1,234")
(test '("~10,'*d" 100) "*******100")
(test '("~10,,'|:d" 12345678) "12|345|678")
(test '("~10,,,2:d" 12345678) "12,34,56,78")
(test '("~14,'*,'|,4:@d" 12345678) "****+1234|5678")
(test '("~10r" 100) "100")
(test '("~2r" 100) "1100100")
(test '("~8r" 100) "144")
(test '("~16r" 100) "64")
(test '("~16,10,'*r" 100) "********64")

; roman numeral test

(test '("~@r" 4) "IV")
(test '("~@r" 19) "XIX")
(test '("~@r" 50) "L")
(test '("~@r" 100) "C")
(test '("~@r" 1000) "M")
(test '("~@r" 99) "XCIX")
(test '("~@r" 1994) "MCMXCIV")

; old roman numeral test

(test '("~:@r" 4) "IIII")
(test '("~:@r" 5) "V")
(test '("~:@r" 10) "X")
(test '("~:@r" 9) "VIIII")

; cardinal/ordinal English number test

(test '("~r" 4) "four")
(test '("~r" 10) "ten")
(test '("~r" 19) "nineteen")
(test '("~r" 1984) "one thousand, nine hundred eighty-four")
(test '("~:r" -1984) "minus one thousand, nine hundred eighty-fourth")

; character test

(test '("~c" #\a) "a")
(test '("~@c" #\a) "#\\a")
(test `("~@c" ,(integer->char 32)) "#\\space")
(test `("~@c" ,(integer->char 0)) "#\\null")
(test `("~@c" ,(integer->char 27)) "#\\escape")
(test `("~@c" ,(integer->char 127)) "#\\delete")
(test `("~@c" ,(integer->char 128)) "#\\x80")
(test `("~@c" ,(integer->char 255)) "#\\xff")
(test '("~65c") "A")
(test '("~7@c") "#\\alarm")
(test '("~:c" #\a) "a")
(test `("~:c" ,(integer->char 1)) "^A")
(test `("~:c" ,(integer->char 27)) "^[")
(test '("~7:c") "^G")
(test `("~:c" ,(integer->char 128)) "#\\x80")
(test `("~:c" ,(integer->char 127)) "#\\x7f")
(test `("~:c" ,(integer->char 255)) "#\\xff")


; plural test

(test '("test~p" 1) "test")
(test '("test~p" 2) "tests")
(test '("test~p" 0) "tests")
(test '("tr~@p" 1) "try")
(test '("tr~@p" 2) "tries")
(test '("tr~@p" 0) "tries")
(test '("~a test~:p" 10) "10 tests")
(test '("~a test~:p" 1) "1 test")

; tilde test

(test '("~~~~") "~~")
(test '("~3~") "~~~")

; whitespace character test

(test '("~%") "
")
(test '("~3%") "


")
(test '("~&") "")
(test '("abc~&") "abc
")
(test '("abc~&def") "abc
def")
;; SLIB incorrectly add an extra Newline.
(test '("~&") "")
(test '("~3&") "

")
(test '("abc~3&") "abc


")
(test '("~|") (string slib:form-feed))
;; SLIB specific, conflicts with CommonLisp: (test '("~_~_~_") "   ") 
;; SLIB specific, conflicts with CommonLisp: (test '("~3_") "   ")
;; SLIB specific, conflicts with CommonLisp: (test '("~/") (string slib:tab))
;; SLIB specific: (test '("~3/") (make-string 3 slib:tab))

; tabulate test

(test '("~0&~3t") "   ")
(test '("~0&~10t") "          ")
(test '("~10t") "          ")
(test '("~0&1234567890~,8tABC")  "1234567890       ABC")
(test '("~0&1234567890~0,8tABC") "1234567890      ABC")
(test '("~0&1234567890~1,8tABC") "1234567890       ABC")
(test '("~0&1234567890~2,8tABC") "1234567890        ABC")
(test '("~0&1234567890~3,8tABC") "1234567890 ABC")
(test '("~0&1234567890~4,8tABC") "1234567890  ABC")
(test '("~0&1234567890~5,8tABC") "1234567890   ABC")
(test '("~0&1234567890~6,8tABC") "1234567890    ABC")
(test '("~0&1234567890~7,8tABC") "1234567890     ABC")
(test '("~0&1234567890~8,8tABC") "1234567890      ABC")
(test '("~0&1234567890~9,8tABC") "1234567890       ABC")
(test '("~0&1234567890~10,8tABC") "1234567890        ABC")
(test '("~0&1234567890~11,8tABC") "1234567890 ABC")
(test '("~0&12345~,8tABCDE~,8tXYZ") "12345    ABCDE   XYZ")
(test '("~,8t+++~,8t===") " +++     ===")
(test '("~0&ABC~,8,'.tDEF") "ABC......DEF")
(test '("~0&~3,8@tABC") "        ABC")
(test '("~0&1234~3,8@tABC") "1234    ABC")
(test '("~0&12~3,8@tABC~3,8@tDEF") "12      ABC     DEF")

; indirection test

(test '("~a ~? ~a" 10 "~a ~a" (20 30) 40) "10 20 30 40")
(test '("~a ~@? ~a" 10 "~a ~a" 20 30 40) "10 20 30 40")

; field test

(test '("~10a" "abc") "abc       ")
(test '("~10@a" "abc") "       abc")
(test '("~10a" "0123456789abc") "0123456789abc")
(test '("~10@a" "0123456789abc") "0123456789abc")

; pad character test

(test '("~10,,,'*a" "abc") "abc*******")
(test '("~10,,,'Xa" "abc") "abcXXXXXXX")
(test '("~10,,,42a" "abc") "abc*******")
(test '("~10,,,'*@a" "abc") "*******abc")
(test '("~10,,3,'*a" "abc") "abc*******")
(test '("~10,,3,'*a" "0123456789abc") "0123456789abc***") ; min. padchar length
(test '("~10,,3,'*@a" "0123456789abc") "***0123456789abc")

; colinc, minpad padding test

(test '("~10,8,0,'*a" 123)  "123********")
(test '("~10,9,0,'*a" 123)  "123*********")
(test '("~10,10,0,'*a" 123) "123**********")
(test '("~10,11,0,'*a" 123) "123***********")
(test '("~8,1,0,'*a" 123) "123*****")
(test '("~8,2,0,'*a" 123) "123******")
(test '("~8,3,0,'*a" 123) "123******")
(test '("~8,4,0,'*a" 123) "123********")
(test '("~8,5,0,'*a" 123) "123*****")
(test '("~8,1,3,'*a" 123) "123*****")
(test '("~8,1,5,'*a" 123) "123*****")
(test '("~8,1,6,'*a" 123) "123******")
(test '("~8,1,9,'*a" 123) "123*********")

; slashify test

(test '("~s" "abc") "\"abc\"")
(test '("~s" "abc \\ abc") "\"abc \\\\ abc\"")
(test '("~a" "abc \\ abc") "abc \\ abc")
(test '("~s" "abc \" abc") "\"abc \\\" abc\"")
(test '("~a" "abc \" abc") "abc \" abc")
(test '("~s" #\space) "#\\space")
(test '("~s" #\newline) "#\\newline")
;; SLIB has: (test '("~s" #\tab) "#\\ht")
(test '("~s" #\tab) "#\\tab")
(test '("~s" #\a) "#\\a")
(test '("~a" (a "b" c)) "(a b c)")

; symbol case force test

(define format:old-scc format:symbol-case-conv)
(set! format:symbol-case-conv string-upcase)
(test-expect-fail 1) ; format:symbol-case-conv not implemented
(test '("~a" abc) "ABC")
(set! format:symbol-case-conv string-downcase)
(test '("~s" abc) "abc")
(set! format:symbol-case-conv string-capitalize)
(test-expect-fail 1) ; format:symbol-case-conv not implemented
(test '("~s" abc) "Abc")
(set! format:symbol-case-conv format:old-scc)

; read proof test

(test `("~:s" ,display)	(format:iobj->str display))
(test `("~:a" ,display) (format:iobj->str display))
(test `("~:a" (1 2 ,display))
      (string-append "(1 2 " (format:iobj->str display) ")"))
(test '("~:a" "abc") "abc")

; internal object case type force test

(set! format:iobj-case-conv string-upcase)
(test-expect-fail 1) ; format:iobj-case-conv not implemented
(test `("~a" ,display) (string-upcase (format:iobj->str display)))
(set! format:iobj-case-conv string-downcase)
(test `("~s" ,display) (string-downcase (format:iobj->str display)))
(set! format:iobj-case-conv string-capitalize)
(test-expect-fail 1) ; format:iobj-case-conv not implemented
(test `("~s" ,display) (string-capitalize (format:iobj->str display)))
(set! format:iobj-case-conv #f)

; continuation line test

(test '("abc~
         123") "abc123")
(test '("abc~
123") "abc123")
(test '("abc~
") "abc")
(test '("abc~:
         def") "abc         def")
(test '("abc~@
         def")
"abc
def")

; flush output (can't test it here really)

(test '("abc ~! xyz") "abc  xyz")

; string case conversion

(test '("~a ~(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc hello world xyz")
(test '("~a ~:(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello World xyz")
(test '("~a ~@(~a~) ~a" "abc" "HELLO WORLD" "xyz") "abc Hello world xyz")
(test '("~a ~:@(~a~) ~a" "abc" "hello world" "xyz") "abc HELLO WORLD xyz")
(test '("~:@(~a~)" (a b c)) "(A B C)")
(test '("~:@(~x~)" 255) "FF")
(test '("~:@(~p~)" 2) "S")
(test `("~:@(~a~)" ,display) (string-upcase (format:iobj->str display)))
(test '("~:(~a ~a ~a~) ~a" "abc" "xyz" "123" "world") "Abc Xyz 123 world")

; variable parameter

(test '("~va" 10 "abc") "abc       ")
(test '("~v,,,va" 10 42 "abc") "abc*******")

; number of remaining arguments as parameter

(test '("~#,,,'*@a ~a ~a ~a" 1 1 1 1) "***1 1 1 1")

; argument jumping

(test '("~a ~* ~a" 10 20 30) "10  30")
(test '("~a ~2* ~a" 10 20 30 40) "10  40")
(test '("~a ~:* ~a" 10) "10  10")
(test '("~a ~a ~2:* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~@* ~a ~a" 10 20) "10 20  10 20")
(test '("~a ~a ~4@* ~a ~a" 10 20 30 40 50 60) "10 20  50 60")

; conditionals

(test '("~[abc~;xyz~]" 0) "abc")
(test '("~[abc~;xyz~]" 1) "xyz")
(test '("~[abc~;xyz~:;456~]" 99) "456")
(test '("~0[abc~;xyz~:;456~]") "abc")
(test '("~1[abc~;xyz~:;456~] ~a" 100) "xyz 100")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]") "no arg")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10) "10")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20) "10 and 20")
(test '("~#[no arg~;~a~;~a and ~a~;~a, ~a and ~a~]" 10 20 30) "10, 20 and 30")
(test '("~:[hello~;world~] ~a" #t 10) "world 10")
(test '("~:[hello~;world~] ~a" #f 10) "hello 10")
(test '("~@[~a tests~]" #f) "")
(test '("~@[~a tests~]" 10) "10 tests")
(test '("~@[~a test~:p~] ~a" 10 done) "10 tests done")
(test '("~@[~a test~:p~] ~a" 1 done) "1 test done")
(test '("~@[~a test~:p~] ~a" 0 done) "0 tests done")
(test '("~@[~a test~:p~] ~a" #f done) " done")
(test '("~@[ level = ~d~]~@[ length = ~d~]" #f 5) " length = 5")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 0) "abc")   ; nested conditionals (irrghh)
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 2) "xyz")
(test '("~[abc~;~[4~;5~;6~]~;xyz~]" 1 2) "6")

; iteration

(test '("~{ ~a ~}" (a b c)) " a  b  c ")
(test '("~{ ~a ~}" ()) "")
(test '("~{ ~a ~5,,,'*a~}" (a b c d)) " a b**** c d****")
(test '("~{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2  c,3 ")
(test '("~2{ ~a,~a ~}" (a 1 b 2 c 3)) " a,1  b,2 ")
(test '("~3{~a ~} ~a" (a b c d e) 100) "a b c  100")
(test '("~0{~a ~} ~a" (a b c d e) 100) " 100")
(test '("~:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d  g,h ")
(test '("~2:{ ~a,~a ~}" ((a b) (c d e f) (g h))) " a,b  c,d ")
(test '("~@{ ~a,~a ~}" a 1 b 2 c 3) " a,1  b,2  c,3 ")
(test '("~2@{ ~a,~a ~} <~a|~a>" a 1 b 2 c 3) " a,1  b,2  <c|3>")
(test '("~:@{ ~a,~a ~}" (a 1) (b 2) (c 3)) " a,1  b,2  c,3 ")
(test '("~2:@{ ~a,~a ~} ~a" (a 1) (b 2) (c 3)) " a,1  b,2  (c 3)")
(test '("~{~}" "<~a,~a>" (a 1 b 2 c 3)) "<a,1><b,2><c,3>")
(test '("~{ ~a ~{<~a>~}~} ~a" (a (1 2) b (3 4)) 10) " a <1><2> b <3><4> 10")

; up and out

(test '("abc ~^ xyz") "abc ")
;; SLIB has: (test '("~@(abc ~^ xyz~) ~a" 10) "ABC  xyz 10")
(test '("~@(abc ~^ xyz~) ~a" 10) "Abc  xyz 10")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p.") "done. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10) "done.  10 warnings. ")
(test '("done. ~^ ~d warning~:p. ~^ ~d error~:p." 10 1)
      "done.  10 warnings.  1 error.")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e f) 10) " a <b> c <d> e <f> 10")
(test '("~{ ~a ~^<~a>~} ~a" (a b c d e) 10) " a <b> c <d> e  10")
(test '("abc~0^ xyz") "abc")
(test '("abc~9^ xyz") "abc xyz")
(test '("abc~7,4^ xyz") "abc xyz")
(test '("abc~7,7^ xyz") "abc")
(test '("abc~3,7,9^ xyz") "abc")
(test '("abc~8,7,9^ xyz") "abc xyz")
(test '("abc~3,7,5^ xyz") "abc xyz")

; complexity tests (oh my god, I hardly understand them myself (see CL std))

(define fmt "Items:~#[ none~; ~a~; ~a and ~a~:;~@{~#[~; and~] ~a~^,~}~].")

(test `(,fmt ) "Items: none.")
(test `(,fmt foo) "Items: foo.")
(test `(,fmt foo bar) "Items: foo and bar.")
(test `(,fmt foo bar baz) "Items: foo, bar, and baz.")
(test `(,fmt foo bar baz zok) "Items: foo, bar, baz, and zok.")

; fixed floating points

(cond
 (format:floats
  (test '("~6,2f" 3.14159) "  3.14")
  (test '("~6,1f" 3.14159) "   3.1")
  (test '("~6,0f" 3.14159) "    3.")
  (test '("~5,1f" 0) "  0.0")
  (test '("~10,7f" 3.14159) " 3.1415900")
  (test '("~10,7f" -3.14159) "-3.1415900")
  (test '("~10,7@f" 3.14159) "+3.1415900")
  (test '("~6,3f" 0.0) " 0.000")
  (test '("~6,4f" 0.007) "0.0070")
  (test '("~6,3f" 0.007) " 0.007")
  (test '("~6,2f" 0.007) "  0.01")
  (test '("~3,2f" 0.007) ".01")
  (test '("~3,2f" -0.007) "-.01")
  (test '("~6,2,,,'*f" 3.14159) "**3.14")
  (test '("~6,3,,'?f" 12345.56789) "??????")
  (test '("~6,3f" 12345.6789) "12345.679")
  (test '("~,3f" 12345.6789) "12345.679")
  (test '("~,3f" 9.9999) "10.000")
  (test '("~6f" 23.4) "  23.4")
  (test '("~6f" 1234.5) "1234.5")
  (test '("~6f" 12345678) "12345678.")
  (test '("~6,,,'?f" 12345678) "??????")
  (test '("~6f" 123.56789) "123.57")
  (test '("~6f" 123.0) " 123.0")
  (test '("~6f" -123.0) "-123.0")
  (test '("~6f" 0.0) "   0.0")
  (test '("~3f" 3.141) "3.1")
  (test '("~2f" 3.141) "3.")
  ;; SLIB incorrectly has: (test '("~1f" 3.141) "3.141")
  (test '("~1f" 3.141) "3.")
  (test '("~f" 123.56789) "123.56789")
  (test '("~f" -314.0) "-314.0")
  (test '("~f" 0.6) "0.6")
  (test '("~f" 0.7) "0.7")
  (test '("~f" 1e4) "10000.0")
  (test '("~f" -1.23e10) "-12300000000.0")
  (test '("~f" 1e-4) "0.0001")
  (test '("~f" -1.23e-10) "-0.000000000123")
  (test '("~@f" 314.0) "+314.0")
  (test '("~,,3f" 0.123456) "123.456")
  (test '("~,,-3f" -123.456) "-0.123456")
  (test '("~5,,3f" 0.123456) "123.5")
))

; exponent floating points

(cond
 (format:floats
  (test '("~e" 3.14159) "3.14159E+0")
  (test '("~e" 0.00001234) "1.234E-5")
  (test '("~e" 0.6) "6.0E-1")
  (test '("~e" 0.7) "7.0E-1")
  (test '("~,,,0e" 0.00001234) "0.1234E-4")
  (test '("~,3e" 3.14159) "3.142E+0")
  (test '("~,3@e" 3.14159) "+3.142E+0")
  (test '("~,3@e" 0.0) "+0.000E+0")
  (test '("~,0e" 3.141) "3.E+0")
  (test '("~,3,,0e" 3.14159) "0.314E+1")
  (test '("~,5,3,-2e" 3.14159) "0.00314E+003")
  (test '("~,5,3,-5e" -3.14159) "-0.00000E+006")
  (test '("~,5,2,2e" 3.14159) "31.4159E-01")
  (test '("~,5,2,,,,'ee" 0.0) "0.00000e+00")
  (test '("~12,3e" -3.141) "   -3.141E+0")
  (test '("~12,3,,,,'#e" -3.141) "###-3.141E+0")
  (test '("~10,2e" -1.236e-4) "  -1.24E-4")
  (test '("~5,3e" -3.141) "-3.141E+0")
  (test '("~5,3,,,'*e" -3.141) "*****")
  ;; SLIB has (test '("~3e" 3.14159) "3.14159E+0") - which is rather dubious.
  (test '("~3e" 3.14159) "3.E+0")
  ;; SLIB has (test '("~4e" 3.14159) "3.14159E+0")
  (test '("~4e" 3.14159) "3.E+0")
  (test '("~5e" 3.14159) "3.E+0")
  (test '("~5,,,,'*e" 3.14159) "3.E+0")
  (test '("~6e" 3.14159) "3.1E+0")
  (test '("~7e" 3.14159) "3.14E+0")
  (test '("~7e" -3.14159) "-3.1E+0")
  (test '("~8e" 3.14159) "3.142E+0")
  (test '("~9e" 3.14159) "3.1416E+0")
  (test '("~9,,,,,,'ee" 3.14159) "3.1416e+0")
  (test '("~10e" 3.14159) "3.14159E+0")
  (test '("~11e" 3.14159) " 3.14159E+0")
  (test '("~12e" 3.14159) "  3.14159E+0")
  (test '("~13,6,2,-5e" 3.14159) " 0.000003E+06")
  (test '("~13,6,2,-4e" 3.14159) " 0.000031E+05")
  (test '("~13,6,2,-3e" 3.14159) " 0.000314E+04")
  (test '("~13,6,2,-2e" 3.14159) " 0.003142E+03")
  (test '("~13,6,2,-1e" 3.14159) " 0.031416E+02")
  (test '("~13,6,2,0e" 3.14159)  " 0.314159E+01")
  (test '("~13,6,2,1e" 3.14159)  " 3.141590E+00")
  (test '("~13,6,2,2e" 3.14159)  " 31.41590E-01")
  (test '("~13,6,2,3e" 3.14159)  " 314.1590E-02")
  (test '("~13,6,2,4e" 3.14159)  " 3141.590E-03")
  (test '("~13,6,2,5e" 3.14159)  " 31415.90E-04")
  (test '("~13,6,2,6e" 3.14159)  " 314159.0E-05")
  (test '("~13,6,2,7e" 3.14159)  " 3141590.E-06")
  (test '("~13,6,2,8e" 3.14159)  "31415900.E-07")
  (test '("~7,3,,-2e" 0.001) ".001E+0")
  (test '("~8,3,,-2@e" 0.001) "+.001E+0")
  (test '("~8,3,,-2@e" -0.001) "-.001E+0")
  (test '("~8,3,,-2e" 0.001) "0.001E+0")
  ;;SLIB incorrectly has:  (test '("~7,,,-2e" 0.001) "0.00E+0")
  (test '("~7,,,-2e" 0.001) ".001E+0")
  (test '("~12,3,1e" 3.14159e12) "   3.142E+12")
  (test '("~12,3,1,,'*e" 3.14159e12) "************")
  (test '("~5,3,1e" 3.14159e12) "3.142E+12")
))

; general floating point (this test is from Steele's CL book)

(cond
 (format:floats
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  0.0314159 0.0314159 0.0314159 0.0314159)
	"  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  0.314159 0.314159 0.314159 0.314159)
	"  0.31   |0.314    |0.314    | 0.31    ")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  3.14159 3.14159 3.14159 3.14159)
	"   3.1   | 3.14    | 3.14    |  3.1    ")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  31.4159 31.4159 31.4159 31.4159)
	"   31.   | 31.4    | 31.4    |  31.    ")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  314.159 314.159 314.159 314.159)
	"  3.14E+2| 314.    | 314.    |  3.14E+2") 
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  3141.59 3141.59 3141.59 3141.59)
	"  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  3.14E12 3.14E12 3.14E12 3.14E12)
	"*********|314.0$+10|0.314E+13| 3.14E+12")
  (test '("~9,2,1,,'*g|~9,3,2,3,'?,,'$g|~9,3,2,0,'%g|~9,2g"
	  3.14E120 3.14E120 3.14E120 3.14E120)
	"*********|?????????|%%%%%%%%%|3.14E+120")
  
  (test '("~g" 0.0) "0.0    ")		; further ~g tests 
  (test '("~8g" 0.1) " 0.1    ")
  (test '("~g" 0.1) "0.1    ")
  (test '("~g" 0.01) "1.0E-2")
  (test '("~g" 123.456) "123.456    ")
  (test '("~g" 123456.7) "123456.7    ")
  (test '("~g" 123456.78) "123456.78    ")
  (test '("~g" 0.9282) "0.9282    ")
  (test '("~g" 0.09282) "9.282E-2")
  (test '("~g" 1) "1.0    ")
  (test '("~g" 12) "12.0    ")
  ))

; dollar floating point

(cond
 (format:floats
  (test '("~$" 1.23) "1.23")
  (test '("~$" 1.2) "1.20")
  (test '("~$" 0.0) "0.00")
  (test '("~$" 9.999) "10.00")
  (test '("~3$" 9.9999) "10.000")
  (test '("~,4$" 3.2) "0003.20")
  (test '("~,4$" 10000.2) "10000.20")
  (test '("~,4,10$" 3.2) "   0003.20")
  (test '("~,4,10@$" 3.2) "  +0003.20")
  (test '("~,4,10:@$" 3.2) "+  0003.20")
  (test '("~,4,10:$" -3.2) "-  0003.20")
  (test '("~,4,10$" -3.2) "  -0003.20")
  (test '("~,,10@$" 3.2) "     +3.20")
  (test '("~,,10:@$" 3.2) "+     3.20")
  (test '("~,,10:@$" -3.2) "-     3.20")
  (test '("~,,10,'_@$" 3.2) "_____+3.20")
  (test '("~,,4$" 1234.4) "1234.40")
))

; complex numbers

(cond 
 (format:complex-numbers
  (test '("~i" 3.0) "3.0+0.0i")
  (test '("~,3i" 3.0) "3.000+0.000i")
  (test `("~7,2i" ,(string->number "3.0+5.0i")) "   3.00  +5.00i")
  (test `("~7,2,1i" ,(string->number "3.0+5.0i")) "  30.00 +50.00i")
  (test `("~7,2@i" ,(string->number "3.0+5.0i")) "  +3.00  +5.00i")
  (test `("~7,2,,,'*@i" ,(string->number "3.0+5.0i")) "**+3.00**+5.00i")
  )) ; note: some parsers choke syntactically on reading a complex
     ; number though format:complex is #f; this is why we put them in
     ; strings 

(test '("{~:<[~3d]~:>}" (2 3)) "{([  2])}")
(test '("{~:<[~3d]~:>}" 23) "{23}")
(test '("~:<~W ~@_~:I~W ~:_~W~1I ~_~W~:>" (defun prod (x y) (* x y)))
      "(defun prod (x y) (* x y))")
(test '("~:<~W.~:I~W.~W~1I.~W~:>" (defun prod (x y) (* x y)))
      "(defun.prod.(x y).(* x y))")

;; From email by Ken Dicky posted to SRFI-48 mailing list 2005-06-07:
(test '("~10,3F" 1.02) "     1.020")
(test '("~10,3F" 1.025) "     1.025")
(test '("~10,3F" 1.0256) "     1.026")
(test '("~10,3F" 1.002) "     1.002")
(test '("~10,3F" 1.0025) "     1.002")
(test '("~10,3F" 1.00256) "     1.003")
(test '("~6,3F" 1/3)  " 0.333") ;;; "  .333" OK
;(test '("~4F" 12) "  12")
(test '("~8,3F" 12.3456) "  12.346")
(test '("~6,3F" 123.3456) "123.346")
(test '("~4,3F" 123.3456) "123.346")
(test-expect-fail 1) ; ~F doesn't properly support complex numbers
(test `("~8,3F" ,(sqrt -3.8)) "0.000+1.949i")
(test '("~6,2F" 32) " 32.00")
;; NB: (not (and (exact? 32.) (integer? 32.)))
#| SRFI-48 results
(test '("~6F" 32.) "   32.") ;; "  32.0" OK
(test '("~6F" 32) "    32")
(test '("~8F" 32e45) "  3.2e46")
(test '("~8,2F" 3.4567e20) " 3.46e20")
(test '("~8,2F" 3.4567e21) " 3.46e21")
(test '("~8,2F" 3.4567e22) " 3.46e22")
(test '("~8,2F" 3.4567e23) " 3.46e23")
(test '("~8,0F" 3.4567e24) "   3.e24")
(test '("~8,1F" 3.4567e24) "  3.5e24")
(test '("~8,2F" 3.4567e24) " 3.46e24")
(test '("~8,3F" 3.4567e24) "3.457e24")
(test '("~8,0F" 3.5567e24) "   4.e24")
(test '("~8,1F" 3.5567e24) "  3.6e24")
(test '("~8,2F" 3.5567e24) " 3.56e24")
(test '("~8F" 32e20) "  3.2e21")
|#
;; Common Lisp results.
(test '("~6F" 32.) "  32.0")
(test '("~8F" 32e17) "3200000000000000000.")
(test '("~8F" 32e-45) "     0.0"); CLisp: ".00000000" SRFI-48: " 3.2e-44"
(test '("~8F" 32e20) "3200000000000000000000.")
;;(expect "   3.2e6" (format "~8F" 32e5)) ;; ok.  converted in input to 3200000.0
(test '("~8F" 32e2) "  3200.0") ;; "   3200." OK for SRFI-48

(test '("~8,2F" 32e10) "320000000000.00") ;; SRFI-48: " 3.20e11"
(test '("~12F" 1.2345) "      1.2345")
(test '("~12,2F" 1.2345) "        1.23")
(test '("~12,3F" 1.2345) "       1.234")
(test `("~20,3F" ,(sqrt -3.8)) "+1.9493588689617927i") ; SRFI-48: "        0.000+1.949i"
(test `("~8,3F" ,(sqrt -3.8)) "+1.9493588689617927i"); SRFI-48: "0.000+1.949i")
(test '("~8,2F" 3.4567e11) "345670000000.00") ; SRFI-48: " 3.46e11")
; (expect "#1=(a b c . #1#)"
;         (format "~w" (let ( (c '(a b c)) ) (set-cdr! (cddr c) c) c)))
(test `("~A~A~&" ,(list->string (list #\newline)) "") "\n")
(test '("~a ~? ~a" a "~s" (new) test) "a new test")
(test '("~a ~?, ~a!" a "~s ~a" (new test) yes) "a new test, yes!")
#|
Does not match implementation - or Common Lisp.
|#
(test '("~10,0F" -3e-4) "       -0.")
(test '("~10,1F" -3e-4) "      -0.0")
(test '("~10,2F" -3e-4) "     -0.00")
(test '("~10,3F" -3e-4) "    -0.000")
(test '("~10,4F" -3e-4) "   -0.0003")
(test '("~10,5F" -3e-4) "  -0.00030")
(test '("~10,3F" 1.02) "     1.020")
(test '("~10,3F" 1.025) "     1.025")
(test '("~10,3F" 1.0256) "     1.026")
(test '("~10,3F" 1.002) "     1.002")
(test '("~10,3F" 1.0025) "     1.002")
(test '("~10,3F" 1.00256) "     1.003")

(test '("<~a~a>" 3 4) "<34>")
(test `("<~a>" ,(values 3 4)) "<3 4>")

; inquiry test

;; SLIB specific: (test '("~:q") format:version)

;(if (not test-verbose) (display "done."))

;(format #t "~%~a Test~:p completed. (~a failure~:p)~2%" total fails)

; eof
(test-end)
