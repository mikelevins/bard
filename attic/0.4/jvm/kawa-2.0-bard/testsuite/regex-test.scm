(test-begin "regex")

(require 'regex)
(require 'pregexp)

; Based on pregexp-test.scm.
; Redefined test in terms of test-equal:
;   (test (expression ...) expected-result ...)
; is translated to
;   (test-equal expected-result (expression ...)) ...

; Also added test syntax variant:
;   (#(f1 args1 ...) #(f2 args2 ...) shared-args ...) expected-result
; This is equivalent to:
;   (test-equal expected-result (f1 args1 ... shared-args ...))
;   (test-equal expected-result (f2 args2 ... shared-args ...))
; It's used to test regex and pregexp versions at once.

;Copyright (c) 1999-2005, Dorai Sitaram.
;All rights reserved.

;Permission to copy, modify, distribute, and use this work or
;a modified copy of this work, for any purpose, is hereby
;granted, provided that the copy includes this copyright
;notice, and in the case of a modified copy, also includes a
;notice of modification.  This work is provided as is, with
;no warranty of any kind.

(define-syntax (test form)
  (syntax-case form ()
    ((test) #!void)
    ((test expr expected . rest)
     (syntax-case (syntax expr) ()
       ((#(f1 a1 ...) #(f2 a2 ...) . args)
	#`(begin
	    #,(gnu.lists.PairWithPosition (syntax->datum (syntax expr))
					 'test-equal
					 #`('expected (f1 a1 ... . args)))
	    #,(gnu.lists.PairWithPosition (syntax->datum (syntax expr))
					 'test-equal
					 #`('expected (f2 a2 ... . args)))
	    (test . rest)))
       (_
	#`(begin
	    #,(gnu.lists.PairWithPosition (syntax->datum (syntax expr))
					 'test-equal
					 #'('expected expr))
	    (test . rest)))))))

;last substantial change 2005-04-24
;last change 2008-04-12

(test
   
  (pregexp "c.r")
  (:sub (:or (:seq #\c :any #\r)))
   
  (#(pregexp-match-positions) #(regex-match-positions) "brain" "bird")
  #f

  (#(pregexp-match-positions "needle")
   #(regex-match-positions #/needle/)
   "hay needle stack")
  ((4 . 10))

  (#(pregexp-match-positions) #(regex-match-positions) "needle"
    "his hay needle stack -- my hay needle stack -- her hay needle stack"
    24 43)
  ((31 . 37))
   
  (#(pregexp-match) #(regex-match) "brain" "bird")
  #f
   
  (#(pregexp-match) #(regex-match) "needle" "hay needle stack")
  ("needle")

  (#(pregexp-split) #(regex-split)
   ":" "/bin:/usr/bin:/usr/bin/X11:/usr/local/bin")
  ("/bin" "/usr/bin" "/usr/bin/X11" "/usr/local/bin")

  (#(pregexp-split) #(regex-split) " " "pea soup")
  ("pea" "soup")

  (pregexp-split "" "smithereens")
  ("s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s")
  (regex-split "" "smithereens")
  ("" "s" "m" "i" "t" "h" "e" "r" "e" "e" "n" "s" "") ; NOTE difference

  (pregexp-split " +" "split pea     soup")
  ("split" "pea" "soup")
  (regex-split " +" "split pea     soup")
  ("split" "pea" "soup")

  (pregexp-split " *" "split pea     soup")
  ("s" "p" "l" "i" "t" "p" "e" "a" "s" "o" "u" "p")
  (regex-split " *" "split pea     soup")
  ("" "s" "p" "l" "i" "t" "" "p" "e" "a" "" "s" "o" "u" "p" "") ; NOTE difference

  (#(pregexp-replace) #(regex-replace) "te" "liberte" "ty")
  "liberty"

  (#(pregexp-replace*) #(regex-replace*) "te" "liberte egalite fraternite" "ty")
  "liberty egality fratyrnity"
   
  (pregexp-match-positions "^contact" "first contact")
  #f
   
  (#(pregexp-match-positions) #(regex-match-positions) "laugh$" "laugh laugh laugh laugh")
  ((18 . 23))
  
  (#(pregexp-match-positions) #(regex-match-positions) "yack\\b" "yackety yack")
  ((8 . 12))
  
  (#(pregexp-match-positions) #(regex-match-positions) "an\\B" "an analysis")
  ((3 . 5))
   
  (#(pregexp-match) #(regex-match) "p.t" "pet")
  ("pet")
   
  (#(pregexp-match) #(regex-match) "\\d\\d" "0 dear, 1 have to read catch 22 before 9")
  ("22")
   
  (#(pregexp-match "[[:alpha:]_]")
   #(regex-match #/[\p{Alpha}_]/)
   "--x--")
  ("x")
   
  (#(pregexp-match "[[:alpha:]_]")
   #(regex-match #/[\p{Alpha}_]/)
   "--_--")
  ("_")
   
  (#(pregexp-match "[[:alpha:]_]")
   #(regex-match #/[\p{Alpha}_]/) "--:--")
  #f
   
  (#(pregexp-match "[:alpha:]")
   #(regex-match #/\p{Alpha}/) "--a--")
  ("a")

  (#(pregexp-match "[:alpha:]")
   #(regex-match #/\p{Alpha}/) "--_--")
  #f
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]*r" "cadaddadddr")
  ((0 . 11))
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]*r" "cr")
  ((0 . 2))
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]+r" "cadaddadddr")
  ((0 . 11))
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]+r" "cr")
  #f
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]?r" "cadaddadddr")
  #f
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]?r" "cr")
  ((0 . 2))
   
  (#(pregexp-match-positions) #(regex-match-positions) "c[ad]?r" "car")
  ((0 . 3))

  (#(pregexp-match) #(regex-match) "[aeiou]{3}" "vacuous")
  ("uou")
   
  (#(pregexp-match) #(regex-match) "[aeiou]{3}" "evolve")
  #f

  (#(pregexp-match) #(regex-match) "[aeiou]{2,3}" "evolve")
  #f

  (#(pregexp-match) #(regex-match) "[aeiou]{2,3}" "zeugma")
  ("eu")

  (#(pregexp-match) #(regex-match) "<.*>" "<tag1> <tag2> <tag3>")
  ("<tag1> <tag2> <tag3>")
   
  (#(pregexp-match) #(regex-match) "<.*?>" "<tag1> <tag2> <tag3>")
  ("<tag1>")
   
  (#(pregexp-match) #(regex-match) "([a-z]+) ([0-9]+), ([0-9]+)" "jan 1, 1970")
  ("jan 1, 1970" "jan" "1" "1970")
   
  (#(pregexp-match) #(regex-match) "(poo )*" "poo poo platter")
  ("poo poo " "poo ")
   
  (#(pregexp-match) #(regex-match) "([a-z ]+;)*" "lather; rinse; repeat;")
  ("lather; rinse; repeat;" " repeat;")
   
  )

;match `month year' or `month day, year'.
;subpattern matches day, if present
(define date-re
  #/([a-z]+) +([0-9]+,)? *([0-9]+)/)
(define date-pre
  (pregexp "([a-z]+) +([0-9]+,)? *([0-9]+)"))

(test
  
  (#(pregexp-match date-pre) #(regex-match date-re) "jan 1, 1970")
  ("jan 1, 1970" "jan" "1," "1970")
  
  (#(pregexp-match date-pre) #(regex-match date-re) "jan 1970")
  ("jan 1970" "jan" #f "1970")
  
  (#(pregexp-replace "_(.+?)_"
		     "the _nina_, the _pinta_, and the _santa maria_"
		     "*\\1*")
   #(regex-replace "_(.+?)_"
		   "the _nina_, the _pinta_, and the _santa maria_"
		   "*$1*"))
  "the *nina*, the _pinta_, and the _santa maria_"
  
  (#(pregexp-replace* "_(.+?)_"
		      "the _nina_, the _pinta_, and the _santa maria_"
		      "*\\1*")
   #(regex-replace* "_(.+?)_"
		    "the _nina_, the _pinta_, and the _santa maria_"
		    "*$1*"))
  "the *nina*, the *pinta*, and the *santa maria*"
  
  (#(pregexp-replace "(\\S+) (\\S+) (\\S+)"
		     "eat to live"
		     "\\3 \\2 \\1")
   #(regex-replace #/(\S+) (\S+) (\S+)/
		  "eat to live"
		  "$3 $2 $1"))
  "live to eat"
  
  (#(pregexp-match) #(regex-match) "([a-z]+) and \\1"
    "billions and billions")
  ("billions and billions" "billions")
  
  (#(pregexp-match) #(regex-match) "([a-z]+) and \\1"
    "billions and millions")
  #f
  
  (#(pregexp-replace* "(\\S+) \\1"
		      "now is the the time for all good men to to come to the aid of of the party"
		      "\\1")
   #(regex-replace* #/(\S+) \1/
		   "now is the the time for all good men to to come to the aid of of the party"
		   "$1"))
  "now is the time for all good men to come to the aid of the party"
  
  (#(pregexp-replace* "(\\d+)\\1"
		      "123340983242432420980980234"
		      "{\\1,\\1}")
   #(regex-replace* #/(\d+)\1/
		    "123340983242432420980980234"
		    "{$1,$1}"))
  "12{3,3}40983{24,24}3242{098,098}0234"
  
  (#(pregexp-match) #(regex-match) "^(?:[a-z]*/)*([a-z]+)$"
   "/usr/local/bin/mzscheme")
  ("/usr/local/bin/mzscheme" "mzscheme")
  
  (#(pregexp-match) #(regex-match) "(?i:hearth)" "HeartH")
  ("HeartH")
  (regex-match #/hearth/i "HeartH")
  ("HeartH")
  
  (#(pregexp-match) #(regex-match) "(?x: a   lot)" "alot")
  ("alot")
  
  (#(pregexp-match) #(regex-match) "(?x: a  \\  lot)" "a lot")
  ("a lot")
  
  (pregexp-match "(?x:
    a \\ man  \\; \\   ; ignore
    a \\ plan \\; \\   ; me
    a \\ canal         ; completely
    )"
    "a man; a plan; a canal")
  ("a man; a plan; a canal")
  
  (#(pregexp-match "(?ix:
    a \\ man  \\; \\   ; ignore
    a \\ plan \\; \\   ; me
    a \\ canal         ; completely
    )")
   #(regex-match "(?i:a man; a plan; a canal)")
    "A Man; a Plan; a Canal")
  ("A Man; a Plan; a Canal")
  
  (#(pregexp-match) #(regex-match) "(?i:the (?-i:TeX)book)"
    "The TeXbook")
  ("The TeXbook")
 
  (#(pregexp-match) #(regex-match) "f(ee|i|o|um)" "a small, final fee")
  ("fi" "i")
   
  (#(pregexp-replace* "([yi])s(e[sdr]?|ing|ation)"
		      "it is energising to analyse an organisation pulsing with noisy organisms"
		      "\\1z\\2")
   #(regex-replace* "([yi])s(e[sdr]?|ing|ation)"
		      "it is energising to analyse an organisation pulsing with noisy organisms"
		      "$1z$2"))
  "it is energizing to analyze an organization pulsing with noisy organisms"
   
  (#(pregexp-match) #(regex-match) "f(?:ee|i|o|um)" "fun for all")
  ("fo")
   
  (#(pregexp-match) #(regex-match) "call|call-with-current-continuation"
    "call-with-current-continuation")
  ("call")
   
  (#(pregexp-match) #(regex-match) "call-with-current-continuation|call"
    "call-with-current-continuation")
  ("call-with-current-continuation")
  
  (#(pregexp-match) #(regex-match) "(?:call|call-with-current-continuation) constrained"
    "call-with-current-continuation constrained")
  ("call-with-current-continuation constrained")
  
  (#(pregexp-match) #(regex-match) "(?>a+)." "aaaa")
  #f
   
  (#(pregexp-match-positions) #(regex-match-positions) "grey(?=hound)"
    "i left my grey socks at the greyhound")
  ((28 . 32))
   
  (#(pregexp-match-positions) #(regex-match-positions) "grey(?!hound)"
    "the gray greyhound ate the grey socks")
  ((27 . 31))
  
  (#(pregexp-match-positions) #(regex-match-positions) "(?<=grey)hound"
    "the hound in the picture is not a greyhound")
  ((38 . 43))
  
  (#(pregexp-match-positions) #(regex-match-positions) "(?<!grey)hound"
    "the greyhound in the picture is not a hound")
  ((38 . 43))
  
  )

#|
(define n0-255
  "(?x:
  \\d          ;  0 through   9
  | \\d\\d     ; 00 through  99
  | [01]\\d\\d ;000 through 199
  | 2[0-4]\\d  ;200 through 249
  | 25[0-5]    ;250 through 255
  )")
|#
(define n0-255
  "(?:\\d|\\d\\d|[01]\\d\\d|2[0-4]\\d|25[0-5])")

(define ip-re1
  (string-append
    "^"        ;nothing before
    n0-255     ;the first n0-255,
    "(?x:"     ;then the subpattern of
    "\\."      ;a dot followed by
    n0-255     ;an n0-255,
    ")"        ;which is
    "{3}"      ;repeated exactly 3 times
    "$"        ;with nothing following
    ))

(test
  
  (#(pregexp-match) #(regex-match) ip-re1
    "1.2.3.4")
  ("1.2.3.4")
  
  (#(pregexp-match) #(regex-match) ip-re1
    "55.155.255.265")
  #f
  
  (#(pregexp-match) #(regex-match) ip-re1
    "0.00.000.00")
  ("0.00.000.00")
  
  )


(define ip-re
  (string-append
    "(?=[1-9])" ;ensure there's a non-0 digit
    ip-re1))

(test
  
  (#(pregexp-match) #(regex-match) ip-re
    "1.2.3.4")
  ("1.2.3.4")
  
  (#(pregexp-match) #(regex-match) ip-re
    "0.0.0.0")
  #f
  
  )

(set! ip-re
  (string-append
    "(?![0.]*$)" ;not just zeros and dots
    ;dot is not metachar inside []
    ip-re1))

(test
  
  (#(pregexp-match) #(regex-match) ip-re
    "1.2.3.4")
  ("1.2.3.4")
  
  (#(pregexp-match) #(regex-match) ip-re
    "0.0.0.0")
  #f
  
  ;misc
  
  (#(pregexp-match) #(regex-match) "a[^a]*b" "glauber")
  ("aub")
  
  (#(pregexp-match) #(regex-match) "a([^a]*)b" "glauber")
  ("aub" "u")
  
  (#(pregexp-match) #(regex-match) "a([^a]*)b" "ababababab")
  ("ab" "")
  
  (#(pregexp-match) #(regex-match) "(?x: s  e  * k )" "seeeeek")
  ("seeeeek")
  
  (#(pregexp-match "(?x: t  ;matches t
    h          ;   matches h
    e           ;;;   matches e
    \\              ; ; ; matches space
    \\;          ;  matches ;
    )")
   #(regex-match "(?x: t  # matches t
    h          #   matches h
    e          #   matches e
    \\         # matches space
    \\;        #  matches ;
    )")
    "the ;")
  ("the ;")

  (#(pregexp-replace* "^(.*)$" "foobar" "\\1abc")
   #(regex-replace* "^(.*)$" "foobar" "$1abc"))
  "foobarabc"

  (#(pregexp-replace* "^(.*)$" "foobar" "abc\\1")
   #(regex-replace* "^(.*)$" "foobar" "abc$1"))
  "abcfoobar"

  (pregexp-replace* "(.*)$" "foobar" "abc\\1")
  "abcfoobar"
  (regex-replace* "(.*)$" "foobar" "abc$1")
  "abcfoobarabc" ;; NOTE difference

  )

(test

  ;PLT bug 6095 from Neil W. Van Dyke
  (pregexp "[a-z-]")
  (:sub (:or (:seq (:one-of-chars (:char-range #\a #\z) #\-))))
  ;
  (pregexp "[-a-z]")
  (:sub (:or (:seq (:one-of-chars #\- (:char-range #\a #\z)))))

  ;PLT bug 6442 from David T. Pierson
  (#(pregexp-match-positions) #(regex-match-positions) "(a(b))?c" "abc")
  ((0 . 3) (0 . 2) (1 . 2))
  ;
  (#(pregexp-match-positions) #(regex-match-positions) "(a(b))?c" "c")
  ((0 . 1) #f #f)

  ;PLT bug 7233 from Edi Weitz
  (#(length (pregexp-match "(a)|(b)" "b"))
   #(length (regex-match "(a)|(b)" "b")))
  3

  ;PLT bug 7232 from Neil Van Dyke
  (pregexp "[-a]")
  (:sub (:or (:seq (:one-of-chars #\- #\a))))
  ;
  (pregexp "[a-]")
  (:sub (:or (:seq (:one-of-chars #\a #\-))))
   
  )

(test-end)
