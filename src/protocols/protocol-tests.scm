;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-tests.scm
;;;; Project:       Bard
;;;; Purpose:       tests of protocol functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (%test-eval expr expected-value)
  (let ((val (%eval expr '())))
    (if (procedure? expected-value)
        (expected-value val)
        (if (equal? val expected-value)
            #t
            #f))))

(define (%run-tests suite-name test-alist)
  (newline)
  (display suite-name)
  (newline)
  (for-each (lambda (item)
              (let* ((expr (car item))
                     (expected (cdr item))
                     (result (if (%test-eval expr expected)
                                 " Success"
                                 " FAILURE")))
                (newline)
                (display (string-append result ": " (object->string expr) " => " 
                                        (cond
                                         ((null? expected) "nothing")
                                         ((eqv? expected #t) "true")
                                         ((eqv? expected #f) "false")
                                         (else (object->string expected)))))
                ))
            test-alist)
  (newline)
  (newline))

;;; ---------------------------------------------------------------------
;;; Anything
;;; ---------------------------------------------------------------------

(%run-tests "Anything protocol"
 `(((something? nothing) . #f)
   ((something? 3) . #t)))

;;; ---------------------------------------------------------------------
;;; Applicable
;;; ---------------------------------------------------------------------

(%run-tests "Applicable protocol"
 `(((applicable? 5) . #f)
   ((applicable? id) . #t)
   ((applicable? print) . #t)
   ((applicable? (method (x) x)) . #t)
   ((applicable? nothing) . #t)
   ((applicable? '(a b c)) . #t)
   ((applicable? "foo") . #t)
   ((applicable? (frame a: 0 b: 1)) . #t)
   
   ((apply version nothing) . ,$bard-version-string)
   ((apply id (list 5)) . 5)
   ((apply odd? (list 5)) . #t)
   ((apply nothing nothing) . ())
   ((apply nothing (list nothing)) . ())
   ((apply nothing (list 1 2 3)) . ())
   ((apply (list 0 1 2 3) (list 1)) . 1)
   ((apply "abc" (list 1)) . #\b)
   ((apply (frame a: 1 b: 2 c: 3) (list b:)) . 2)
   ))

;;; ---------------------------------------------------------------------
;;; As
;;; ---------------------------------------------------------------------

(%run-tests "As protocol"
 `(
   ((as ,<cons> "foo") . ,(list #\f #\o #\o))
   ((as ,<string> (list #\f #\o #\o)) . "foo")
   ))

;;; ---------------------------------------------------------------------
;;; Atom
;;; ---------------------------------------------------------------------

(%run-tests "Atom protocol"
 `(
   ((atom? nothing) . #t)
   ((atom? 5) . #t)
   ((atom? (list 1 2 3)) . #f)
   ((atom? "foo") . #f)
   ((atom? (frame a: 1 b: 2)) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(%run-tests "Boolean protocol"
 `(
   ((boolean? 5) . #f)
   ((boolean? true) . #t)
   ((boolean? false) . #t)
   ((false? false) . #t)
   ((false? true) . #f)
   ((true? false) . #f)
   ((true? true) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

(%run-tests "Character protocol"
 `(
   ((character? 2) . #f)
   ((character? #\A) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Comparable
;;; ---------------------------------------------------------------------

(%run-tests "Comparable protocol"
 `(
   ((comparable? 5 "foo") . #f)
   ((comparable? 2 3) . #t)
   ((comparable? 2 888888888888) . #t)
   ((comparable? 888888888888 2) . #t)
   ((comparable? 888888888888 999999999999) . #t)
   ((comparable? #\c #\space) . #t)
   ((comparable? "" "Foo") . #t)
   
   ((= 3 (+ 1 2)) . #t)
   ((= 5 "foo") . #f)
   
   ((> 3 2 1) . #t)
   ((> 888888888888 2 1) . #t)
   ((> #\C #\B #\A) . #t)
   ((> #\C #\A #\B) . #f)
   ((> "Foo" "Bar") . #t)
   ((> "Foo" "Bar" "Arg") . #t)

   ((< 3 2 1) . #f)
   ((< 888888888888 2 1) . #f)
   ((< #\C #\B #\A) . #f)
   ((< #\A #\B #\C) . #t)
   ((< "Bar" "Foo") . #t)
   ((< "Foo" "Bar" "Arg") . #f)

   ((>= 3 3 2) . #t)
   ((>= 999999999999 999999999999 888888888888) . #t)
   ((>= #\C #\B #\B) . #t)
   ((>= "Bar" "Foo") . #f)
   ((>= "Bar" "Bar") . #t)

   ((<= 2 3 3) . #t)
   ((<= 888888888888 999999999999 999999999999) . #t)
   ((<= #\B #\B #\C) . #t)
   ((<= "Bar" "Foo") . #t)
   ((<= "Bar" "Bar") . #t)))

;;; ---------------------------------------------------------------------
;;; Float
;;; ---------------------------------------------------------------------

(%run-tests "Float protocol"
 `(
   ((float? 2) . #f)
   ((float? 1.2) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; ForeignValue
;;; ---------------------------------------------------------------------

(%run-tests "ForeignValue protocol"
 `(
   ((foreign-value? 2) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; Frame
;;; ---------------------------------------------------------------------

(%run-tests "Frame protocol"
 `(
   ((frame? 2) . #f)
   ((frame? nothing) . #t)
   ((frame? '(a b c)) . #t)
   ((frame? "foo") . #t)
   ((frame? (frame a: 1 b: 2)) . #t)

   ((contains-key? nothing 0) . #f)
   ((contains-key? '(a b c) 0) . #t)
   ((contains-key? '(a b c) 10) . #f)
   ((contains-key? "abc" 0) . #t)
   ((contains-key? "abc" 10) . #f)
   ((contains-key? (frame a: 1 b: 2 c: 3) b:) . #t)
   ((contains-key? (frame a: 1 b: 2 c: 3) x:) . #f)
   
   ((contains-value? nothing 0) . #f)
   ((contains-value? '(a b c) 'c) . #t)
   ((contains-value? '(a b c) 10) . #f)
   ((contains-value? "abc" #\b) . #t)
   ((contains-value? "abc" 10) . #f)
   ((contains-value? (frame a: 1 b: 2 c: 3) 3) . #t)
   ((contains-value? (frame a: 1 b: 2 c: 3) 10) . #f)
   ((contains-value? (frame a: 1 b: 2 c: 3) 9 (method (x y) (odd? x))) . #t)

   ((get nothing 0) . ,(%nothing))
   ((get '(a b c) 0) . a)
   ((get "abc" 0) . #\a)
   ((get (frame a: 1 b: 2 c: 3) c:) . 3)
   ((get (frame a: 1 b: 2 c: 3) z:) . ,(%nothing))

   ((keys nothing) . ,(%nothing))
   ((keys '(a b c)) . (0 1 2))
   ((keys "") . ,(%nothing))
   ((keys "abc") . (0 1 2))
   ((keys (frame)) . ,(%nothing))
   ((keys (frame a: 1 b: 2 c: 3)) . (a: b: c:))

   ((merge nothing nothing) . ,(%nothing))
   ((merge nothing '(a b c)) . (a b c))
   ((merge nothing "Fred") . "Fred")
   ((merge nothing (frame a: 1 b: 2 c: 3)) . ,(->frame a: 1 b: 2 c: 3))
   ((merge '(a b c) nothing) . (a b c))
   ((merge "Fred" nothing) . "Fred")
   ((merge (frame a: 1 b: 2 c: 3) nothing) . ,(->frame a: 1 b: 2 c: 3))
   ((merge '(a b c) '(0 1)) . (0 1 c))
   ((merge '(a b c) '(0 1 2 3)) . (0 1 2 3))
   ((merge '(a b c) "a") . (#\a b c))
   ((merge '(a b c) "abcd") . "abcd")
   ((merge '(a b c) (frame)) . (a b c))
   ((merge '(a b c) (frame '1 'BEE)) . (a BEE c))
   ((merge '(a b c) (frame '26 'ZEBRA)) . ,(->frame 0 'a 1 'b 2 'c 26 'ZEBRA))
   ((merge "abcd" '(a b c)) . (a b c #\d))
   ((merge "Fred" "Fl") . "Fled")
   ((merge "Fred" "Freddy") . "Freddy")
   ((merge "abc" (frame)) . "abc")
   ((merge "abc" (frame 1 #\B)) . "aBc")
   ((merge "abc" (frame 26 #\B)) . ,(->frame 0 #\a 1 #\b 2 #\c 26 #\B))
   ((merge "abc" (frame 1 'boggle)) . ,(->frame 0 #\a 2 #\c 1 'boggle))
   ((merge (frame a: 1 b: 2) '(0 1)) . ,(->frame a: 1 b: 2 0 0 1 1))
   ((merge (frame a: 1 b: 2) "abc") . ,(->frame a: 1 b: 2 0 #\a 1 #\b 2 #\c))
   ((merge (frame a: 1 b: 2)(frame c: 3)) . ,(->frame a: 1 b: 2 c: 3))
   ((merge (frame a: 1 b: 2)(frame b: 'TWO c: 3)) . ,(->frame a: 1 b: 'TWO c: 3))
   ((merge (frame a: 1 b: 2 c: 3)(frame b: 'TWO)) . ,(->frame a: 1 c: 3 b: 'TWO))

   ((put 5 name: 'five) . ,(->frame type: <fixnum> value: 5 name: 'five))
   ((put nothing 0 'zero) . (zero))
   ((put "" 0 #\A) . "A")
   ((put "abc" 0 #\A) . "Abc")
   ((put "abc" 2 #\A) . "abA")
   ((put (frame) a: 1) . ,(->frame a: 1))
   ((put (frame a: 1 b: 2) c: 3) . ,(->frame a: 1 b: 2 c: 3))
   ((put (frame a: 1 b: 2 c: 3) b: 'THREE) . ,(->frame a: 1 c: 3 b: 'THREE))
   
   ((vals nothing) . ,(%nothing))
   ((vals '(0 1 2)) . (0 1 2))
   ((vals "foobar") . (#\f #\o #\o #\b #\a #\r))
   ((vals (frame a: 1 b: 2 c: 3)) . (1 2 3))
   ))

;;; ---------------------------------------------------------------------
;;; Function
;;; ---------------------------------------------------------------------

(%run-tests "Function protocol"
 `(
   ((function? 2) . #f)
   ((function? =) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; IOStream
;;; ---------------------------------------------------------------------

(%run-tests "IOStream protocol"
 `(
   ((iostream? 2) . #f)
   ((iostream? (current-input)) . #t)

   ((show 10) . "10")
   ))

;;; ---------------------------------------------------------------------
;;; Integer
;;; ---------------------------------------------------------------------

(%run-tests "Integer protocol"
 `(
   ((integer? 2) . #t)
   ((integer? 2.3) . #f)
   ((integer? 2/3) . #f)
   ((integer? =) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; Keyword
;;; ---------------------------------------------------------------------

(%run-tests "Keyword protocol"
 `(
   ((keyword? 2) . #f)
   ((keyword? Foo:) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

(%run-tests "List protocol"
 `(
   ((list? 2) . #f)
   ((list? nothing) . #t)
   ((list? '(0 1 2)) . #t)
   ((list? "foo") . #t)
   ((list? (frame a: 1 b: 2)) . #t)

   ((add-first 2 nothing) . (2))
   ((add-first 2 '(3 4 5)) . (2 3 4 5))
   ((add-first #\A "bcd") . "Abcd")
   ((add-first '(a: 1) (frame b: 2 c: 3)) . ,(->frame a: 1 b: 2 c: 3))

   ((add-last nothing 2) . (2))
   ((add-last '(3 4 5) 2) . (3 4 5 2))
   ((add-last "bcd" #\A) . "bcdA")
   ((add-last (frame b: 2 c: 3) '(a: 1)) . ,(->frame b: 2 c: 3 a: 1))

   ((any nothing) . ,(%nothing))
   ((any '(0 1 2)) . ,(lambda (val)(member val '(0 1 2))))
   ((any "abc") . ,(lambda (val)(member val '(#\a #\b #\c))))
   ((any (frame a: 1 b: 2 c: 3)) . ,(lambda (val)(member (%frame-slot-key val) '(a: b: c:))))
   
   ((append nothing nothing) . ,(%nothing))
   ((append nothing '(0 1 2)) . (0 1 2))
   ((append nothing "foo") . "foo")
   ((append nothing (frame a: 1 b: 2)) . ,(->frame a: 1 b: 2))
   ((append '(0 1 2) '(3 4 5)) . (0 1 2 3 4 5))
   ((append '(0 1 2) nothing) . (0 1 2))
   ((append '(0 1 2) "abc") . (0 1 2 #\a #\b #\c))
   ((append '(#\A #\B #\C) "abc") . "ABCabc")
   ((append '(0 1 2) (frame 3 'three)) . (0 1 2 (3 three)))
   ((append '((a 0) (b 1) (c 2)) (frame 'd 3)) . ,(->frame 'a 0 'b 1 'c 2 'd 3))
   ((append "foo" nothing) . "foo")
   ((append "foo" '(#\A #\B #\C)) . "fooABC")
   ((append "foo" "Bar") . "fooBar")
   ((append "foo" (frame a: 1 b: 2)) . (#\f #\o #\o (a: 1)(b: 2)))
   ((append (frame a: 1 b: 2) nothing) . ,(->frame a: 1 b: 2))
   ((append (frame 3 'three) '(0 1 2)) . ((3 three) 0 1 2))
   ((append (frame a: 1 b: 2) "foo") . ((a: 1)(b: 2) #\f #\o #\o))
   ((append (frame a: 1 b: 2) (frame c: 3 d: 4)) . ,(->frame a: 1 b: 2 c: 3 d: 4))
   ((append (frame a: 1 b: 2) (frame b: 'TWO c: 3)) . ,(->frame a: 1 b: 'TWO c: 3))
   ((append (frame a: 1 b: 2 c: 3) (frame b: 'TWO)) . ,(->frame a: 1 c: 3 b: 'TWO))

   ((contains? nothing 'foo) . #f)
   ((contains? '(0 1 2 3) 3) . #t)
   ((contains? '(0 1 2 3) 9 (method (x y)(odd? x))) . #t)
   ((contains? "abcdef" #\e) . #t)
   ((contains? "abcdEf" #\e) . #f)
   ((contains? (frame a: 1 b: 2 c: 3) (list b: 2)) . #t)
   ((contains? (frame a: 1 b: 2 c: 3) b: (method (x y)(= x (first y)))) . #t)

   ((difference nothing nothing) . ,(%nothing))
   ((difference nothing '(a b c)) . ,(%nothing))
   ((difference nothing "abc") . ,(%nothing))
   ((difference nothing (frame b: 1)) . ,(%nothing))
   ((difference '(a b c) nothing) . (a b c))
   ((difference "abc" nothing) . "abc")
   ((difference (frame b: 1) nothing) . ,(->frame b: 1))
   ((difference '(a b c) '(c)) . (a b))
   ((difference '(a b c) "bc") . (a b c))
   ((difference '(#\a #\b #\c) "bc") . "a")
   ((difference "abc" "b") . "ac")
   ((difference (frame a: 0 b: 1 c: 2) (frame b: 1)) . ,(->frame a: 0 c: 2))

   ((drop 2 '(a b c d e)) . (c d e))
   ((drop 3 "abcdefgh") . "defgh")
   ((drop 1 (frame a: 0 b: 1 c: 2)) . ,(->frame b: 1 c: 2))
   
   ((drop-before odd? '(0 2 4 6 8 9 10 11 12)) . (9 10 11 12))
   ((drop-before (method (c)(= c #\d)) "abcdefghij") . "defghij")
   ((drop-before (method (s)(= (first s) c:)) (frame a: 1 b: 2 c: 3 d: 4 e: 5)) . ,(->frame c: 3 d: 4 e: 5))

   ((element '(a b c d e) 2) . c)
   ((element "abcedef" 2) . #\c)
   ((element (frame a: 1 b: 2 c: 3 d: 4) 2) . (c: 3))

   ((empty? nothing) . #t)
   ((empty? '(a b c d e)) . #f)
   ((empty? "") . #t)
   ((empty? "abcedef") . #f)
   ((empty? (frame)) . #t)
   ((empty? (frame a: 1 b: 2 c: 3 d: 4)) . #f)

   ((every? odd? nothing) . #t)
   ((every? odd? '(1 3 5 7)) . #t)
   ((every? odd? '(1 3 5 7 8)) . #f)
   ((every? character? "") . #t)
   ((every? character? "foobar") . #t)
   ((every? (method (s)(number? (second s))) (frame a: 1 b: 2 c: 3 d: 4)) . #t)

   ((filter odd? nothing) . ,(%nothing))
   ((filter odd? '(0 1 2 3 4 5 6 7 8 9)) . (1 3 5 7 9))
   ((filter (method (c)(not (= c #\o))) "Frobbozz") . "Frbbzz")
   ((filter (method (s)(odd? (second s))) (frame a: 1 b: 2 c: 3 d: 4)) . ,(->frame a: 1 c: 3))

   ((find odd? nothing) . ,(%nothing))
   ((find odd? '(0 1 2 3 4 5)) . 1)
   ((find (method (c)(= c #\d)) "abcdefghijklmnop") . #\d)
   ((find (method (s)(odd? (second s))) (frame a: 0 b: 1 c: 2)) . (b: 1))

   ((first nothing) . ,(%nothing))
   ((first '(0 1 2 3 4 5)) . 0)
   ((first "abcdefghijklmnop") . #\a)
   ((first (frame a: 0 b: 1 c: 2)) . (a: 0))

   ((head nothing) . ,(%nothing))
   ((head '(0 1 2 3 4 5)) . 0)
   ((head "abcdefghijklmnop") . #\a)
   ((head (frame a: 0 b: 1 c: 2)) . (a: 0))

   ((interleave nothing nothing) . ,(%nothing))
   ((interleave nothing '(a b c)) . ,(%nothing))
   ((interleave nothing "abc") . ,(%nothing))
   ((interleave nothing (frame a: 1 b: 2 c: 3)) . ,(%nothing))
   ((interleave '(a b c) nothing) . ,(%nothing))
   ((interleave "abc" nothing) . ,(%nothing))
   ((interleave (frame a: 1 b: 2 c: 3) nothing) . ,(%nothing))
   ((interleave '(a b c) '(1 2 3)) . (a 1 b 2 c 3))
   ((interleave "abc" "123") . "a1b2c3")
   ((interleave (frame a: 1 c: 3)(frame b: 2 d: 4)) . ,(->frame a: 1 b: 2 c: 3 d: 4))
   ((interleave "abc" '(#\1 #\2 #\3)) . "a1b2c3")
   ((interleave "abc" (frame a: 1 c: 3)) . (#\a (a: 1) #\b (c: 3)))
   ((interleave '(a b c) (frame a: 1 c: 3)) . (a (a: 1) b (c: 3)))
   ((interleave (frame a: 1 c: 3) '(a b c)) . ((a: 1) a (c: 3) b))
   ((interleave (frame a: 1 c: 3) "abc") . ((a: 1) #\a (c: 3) #\b))

   ((interpose nothing nothing) . ,(%nothing))
   ((interpose nothing '(a b c)) . (a ,(%nothing) b ,(%nothing) c))
   ((interpose 0 '(a b c)) . (a 0 b 0 c))
   ((interpose #\space "abcd") . "a b c d")

   ((intersection nothing nothing) . ,(%nothing))
   ((intersection nothing '(a b c)) . ,(%nothing))
   ((intersection nothing "abc") . ,(%nothing))
   ((intersection nothing (frame a: 1 b: 2 c: 3)) . ,(%nothing))
   ((intersection '(a b c) '(b c d)) . (b c))
   ((intersection '(#\a #\b #\c) "bcd") . (#\b #\c))
   ((intersection '((a: 1)(b: 2)(c: 3)) (frame b: 2 c: 3 d: 4)) . ((b: 2)(c: 3)))
   ((intersection "bcd" '(#\a #\b #\c)) . "bc")
   ((intersection "abc" (frame)) . ,(%nothing))
   ((intersection (frame b: 2 c: 3 d: 4) '((a: 1)(b: 2)(c: 3))) . ,(->frame b: 2 c: 3))
   ((intersection (frame a: 1 b: 2 c: 3) (frame b: 2 c: 3 d: 4)) . ,(->frame b: 2 c: 3))

   ((last nothing) . ,(%nothing))
   ((last '(a b c d)) . d)
   ((last "abcd") . #\d)
   ((last (frame a: 1 b: 2 c: 3)) . (c: 3))

   ((length nothing) . 0)
   ((length '(a b c d)) . 4)
   ((length "abcd") . 4)
   ((length (frame a: 1 b: 2 c: 3)) . 3)
   ))

;;; ---------------------------------------------------------------------
;;; Method
;;; ---------------------------------------------------------------------

(%run-tests "Method protocol"
 `(
   ((method? 2) . #f)
   ((method? (method (x) x)) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Name
;;; ---------------------------------------------------------------------

(%run-tests "name protocol"
 `(
   ((name? 2) . #f)
   ((name? Foo:) . #t)
   ((name? 'foo) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Null
;;; ---------------------------------------------------------------------

(%run-tests "Null protocol"
 `(
   ((nothing? 2) . #f)
   ((nothing? nothing) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Number
;;; ---------------------------------------------------------------------

(%run-tests "Number protocol"
 `(
   ((number? 2) . #t)
   ((number? 888888888888) . #t)
   ((number? 2.3) . #t)
   ((number? 2/3) . #t)
   ((number? #\1) . #f)
   ((number? "1") . #f)
   ((number? =) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; PrimitiveValue
;;; ---------------------------------------------------------------------

(%run-tests "PrimitiveValue protocol"
 `(
   ((primitive-value? undefined) . #t)
   ((primitive-value? nothing) . #t)
   ((primitive-value? #\c) . #t)
   ((primitive-value? true) . #t)
   ((primitive-value? 'foo) . #t)
   ((primitive-value? Bar:) . #t)
   ((primitive-value? 2.3) . #t)
   ((primitive-value? 2/3) . #t)
   ((primitive-value? 3) . #t)
   ((primitive-value? 888888888888) . #t)
   ((primitive-value? current-input) . #t)
   ((primitive-value? (list 1 2 3)) . #t)
   ((primitive-value? "123") . #t)
   ((primitive-value? (singleton 3)) . #f)
   ((primitive-value? (method (x) x)) . #f)
   ((primitive-value? (function 'test)) . #f)
   ((primitive-value? (current-input)) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; Procedure
;;; ---------------------------------------------------------------------

(%run-tests "Procedure protocol"
 `(
   ((procedure? 2) . #f)
   ((procedure? print) . #t)
   ((procedure? =) . #t)
   ((procedure? (method (x) x)) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Ratio
;;; ---------------------------------------------------------------------

(%run-tests "Ratio protocol"
 `(
   ((ratio? 2) . #f)
   ((ratio? 2.3) . #f)
   ((ratio? 2/3) . #t)
   ((ratio? =) . #f)
   ))

;;; ---------------------------------------------------------------------
;;; StructureValue
;;; ---------------------------------------------------------------------

(%run-tests "StructureValue protocol"
 `(
   ((structure-value? 2) . #f)
   ((structure-value? (current-input)) . #t)
   ((structure-value? (method (x) x)) . #t)
   ))


;;; ---------------------------------------------------------------------
;;; Symbol
;;; ---------------------------------------------------------------------

(%run-tests "Symbol protocol"
 `(
   ((symbol? 2) . #f)
   ((symbol? 'Foo) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Text
;;; ---------------------------------------------------------------------

(%run-tests "Text protocol"
 `(
   ((text? 2) . #f)
   ((text? "Foo") . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Type
;;; ---------------------------------------------------------------------

(%run-tests "Type protocol"
 `(
   ((type? 2) . #f)
   ((type? (singleton 2)) . #t)
   ((type? <cons>) . #t)
   ((type? Anything) . #t)
   ))

;;; ---------------------------------------------------------------------
;;; Undefined
;;; ---------------------------------------------------------------------

(%run-tests "Undefined protocol"
 `(
   ((undefined? 2) . #f)
   ((undefined? undefined) . #t)
   ))
