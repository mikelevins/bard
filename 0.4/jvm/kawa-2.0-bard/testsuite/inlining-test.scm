(module-static #t)

(define-namespace Repl "class:kawa.repl")
(define-namespace Repl2 <kawa.repl>)

(define (set-home1 (x :: <String>)) (set! (<kawa.repl>:.homeDirectory) x))
(define (set-home2 (x :: <String>)) (set! <kawa.repl>:homeDirectory x))
(define (set-home3 (x :: <String>)) (set! (Repl:.homeDirectory) x))
(define (set-home4 (x :: <String>)) (set! Repl:homeDirectory x))
(define (set-home5 (x :: <String>)) (set! (kawa.repl:.homeDirectory) x))
(define (set-home6 (x :: <String>)) (set! kawa.repl:homeDirectory x))
(define (set-home7 (x :: <String>)) (set! (Repl2:.homeDirectory) x))
(define (set-home8 (x :: <String>)) (set! Repl2:homeDirectory x))

(define (get-home1) (<kawa.repl>:.homeDirectory))
(define (get-home2) <kawa.repl>:homeDirectory)
(define (get-home3) (Repl:.homeDirectory))
(define (get-home4) Repl:homeDirectory)
(define (get-home5) (kawa.repl:.homeDirectory))
(define (get-home6) kawa.repl:homeDirectory)
(define (get-home7) (Repl2:.homeDirectory))
(define (get-home8) Repl2:homeDirectory)

(define-namespace Pair1 <pair>)
(define-namespace Pair2 <gnu.lists.Pair>)
(define-namespace Pair3 "class:gnu.lists.Pair")

(define (set-car1 (p <pair>) x)  (set! (*:.car p) x))
(define (set-car2 (p <pair>) x)  (set! (<pair>:.car p) x))
(define (set-car3 (p <pair>) x)  (set! (gnu.lists.Pair:.car p) x))
(define (set-car4 (p <pair>) x)  (set! (<gnu.lists.Pair>:.car p) x))
(define (set-car5 (p <pair>) x)  (set! (Pair1:.car p) x))
(define (set-car6 (p <pair>) x)  (set! (Pair2:.car p) x))
(define (set-car7 (p <pair>) x)  (set! (Pair3:.car p) x))
(define (set-car8 (p <pair>) x)  (set!  p:car x))
(define (set-car9 p x)  (set! (Pair1:.car p) x))

(define (get-car1 (p <pair>))  (*:.car p))
(define (get-car2 (p <pair>))  (<pair>:.car p))
(define (get-car3 (p <pair>))  (gnu.lists.Pair:.car p))
(define (get-car4 (p <pair>))  (<gnu.lists.Pair>:.car p))
(define (get-car5 (p <pair>))  (Pair1:.car p))
(define (get-car6 (p <pair>))  (Pair2:.car p))
(define (get-car7 (p <pair>))  (Pair3:.car p))
(define (get-car8 (p <pair>))  p:car)
(define (get-car9 p)  (Pair3:.car p))
(define (get-car10 p)  (<pair>:.car p))
(define (get-car11 p::pair) (car p))

(define (is-pair1 x) (<pair>:instance? x))
(define (is-pair2 x) (gnu.lists.Pair:instance? x))
(define (is-pair3 x) (<gnu.lists.Pair>:instance? x))
(define (is-pair4 x) (Pair1:instance? x))
(define (is-pair5 x) (Pair2:instance? x))
(define (is-pair6 x) (Pair3:instance? x))
(define (is-pair7 x) (instance? x <pair>))
(define (is-pair9 x) (instance? x <gnu.lists.Pair>))
(define (is-pair10 x) (instance? x Pair1:<>))
(define (is-pair11 x) (instance? x Pair2:<>))
(define (is-pair12 x) (instance? x Pair3:<>))
(define (is-pair13 x) (gnu.lists.Pair? x))
(define (is-pair14 x) (Pair1? x))
(define (is-pair15 x) (Pair2? x))
(define (is-pair16 x) (Pair3? x))

(define (cast-to-pair1 x) (<pair>:@ x))
(define (cast-to-pair2 x) (->gnu.lists.Pair x))
(define (cast-to-pair3 x) (<gnu.lists.Pair>:@ x))
(define (cast-to-pair4 x) (Pair1:@ x))
(define (cast-to-pair5 x) (Pair2:@ x))
(define (cast-to-pair6 x) (->Pair3 x))
(define (cast-to-pair7 x) (as <pair> x))
(define (cast-to-pair9 x) (as <gnu.lists.Pair> x))
(define (cast-to-pair10 x) (as Pair1:<> x))
(define (cast-to-pair11 x) (as Pair2:<> x))
(define (cast-to-pair12 x) (as Pair3:<> x))

(define (new-pair1 x y) (<pair>:new x y))
(define (new-pair2 x y) (gnu.lists.Pair:new x y))
(define (new-pair3 x y) (<gnu.lists.Pair>:new x y))
(define (new-pair4 x y) (Pair1:new x y))
(define (new-pair5 x y) (Pair2:new x y))
(define (new-pair6 x y) (Pair3:new x y))
(define (new-pair7 x y) (make <pair> x y))
(define (new-pair9 x y) (make <gnu.lists.Pair> x y))
(define (new-pair10 x y) (make Pair1:<> x y))
(define (new-pair11 x y) (make Pair2:<> x y))
(define (new-pair12 x y) (make Pair3:<> x y))

(define (is-empty1 (p <pair>)) ;; OK
  (*:isEmpty p))

(define (make-iarr1 (n :: <int>)) (make <int[]> size: n))
(define (make-iarr2 (n :: <int>)) (<int[]> size: n))
(define (make-iarr3 (n :: <int>)) (<int[]> size: n 3 4 5))

(define (length-iarr1 (arr :: <int[]>)) :: <int>
  (field arr 'length))
(define (length-iarr2(arr :: <int[]>)) :: <int>
  (*:.length arr))
(define (length-iarr3 (arr :: <int[]>)) :: <int>
  arr:length)
(define (get-iarr1 (arr :: <int[]>) (i :: <int>)) :: <int>
  (arr i))

(define (set-iarr1 (arr :: <int[]>) (i :: <int>) (val :: <int>)) :: <void>
  (set! (arr i) val))

#|
(define (car1 (x :: <pair>)) ;; OK
  (*:.car x))
(define (get-ns str)
  (<gnu.mapping.Namespace>:getInstance str))

(define (xcarx (p <pair>))
  (p:isEmpty))
;  (*:isEmpty p))
;  (*:.car p))
;(define-alias xx #,(namespace "XX"))
(define TWO xx:TWO)
|#
(define-simple-class <Int> ()
  (value :: <int>)
  ((toHex)
   (<java.lang.Integer>:toHexString value))
  ((toHex x) allocation: 'static
   (<java.lang.Integer>:toHexString x))
  ((toHex x) allocation: 'static
   (<java.lang.Integer>:toHexString x)))
(define (tohex1 x)
  (<Int>:toHex x))
(define (tohex2 (x :: <Int>))
  (invoke x 'toHex))
(define (tohex3 (x :: <Int>))
  (x:toHex))

(define (varargs1)
  (invoke gnu.math.IntNum 'getMethod "valueOf"
 java.lang.String java.lang.Integer:TYPE))
(define (varargs2 (argtypes :: java.lang.Class[]))
  (invoke gnu.math.IntNum 'getMethod "valueOf" argtypes))
(define (varargs3 argtypes)
  (invoke gnu.math.IntNum 'getMethod "valueOf" argtypes))


(define (top-level-recurse1 x::pair)
  (set-car! x 123) 
  (top-level-recurse1 x))

(define (top-level-recurse2 a b)
  (top-level-recurse2 b a))

(define-namespace xx "XX")
(define xx:two 222)
(define list-two (list 'xx:Two))

(define (factoriali1 x :: int) :: int
  (if (< x 1) 1
      (* x (factoriali1 (- x 1)))))
(define (factoriali2 x :: <int>) :: <int>
  (if (< x 1) 1
      (* x (factoriali2 (- x 1)))))
(define (factoriall1 x :: long) :: long
  (if (< x 1) 1
      (* x (factoriall1 (- x 1)))))
(define (factorialI1 x :: <integer>) :: <integer>
  (if (< x 1) 1
      (* x (factorialI1 (- x 1)))))

(define (plus-lambda1) :: int
  ((lambda (x y) (+ x y)) 3 4))

(define (first-negative (vals :: double[])) :: double
  (let ((count vals:length))
    (call-with-current-continuation
     (lambda (exit)
       (do ((i :: int 0 (+ i 1)))
	   ((= i count)
	    0)
	   (let ((x (vals i)))
	     (if (< x 0)
		 (exit x))))))))

(define (inline-two-calls (x :: int)) :: int
  (define (f (w :: int)) (+ w 10))
  (if (> x 0)
      (let ((y1 (+ x 1)))
	(f y1))
      (let ((y2 (+ x 2)))
	(f y2))))

(define (inline-two-functions x)
  (letrec ((f (lambda ()
                (if x (f) (g))))
           (g (lambda ()
                (if x (g) (f)))))
    (f)))

(define (check-even (x :: int)) ::boolean
  (letrec ((even?
	    (lambda ((n1 :: int))
	      (if (= n1 0)
		  #t
		  (odd? (- n1 1)))))
	   (odd?
	    (lambda ((n2 :: int))
	      (if (= n2 0)
		  #f
		  (even? (- n2 1))))))
    (even? x)))

;; Same as check-even, but without return-type specifier
(define (check-even-unspec-return (x :: int))
  (letrec ((even?
	    (lambda ((n1 :: int))
	      (if (= n1 0)
		  #t
		  (odd? (- n1 1)))))
	   (odd?
	    (lambda ((n2 :: int))
	      (if (= n2 0)
		  #f
		  (even? (- n2 1))))))
    (even? x)))

(define (constant-propagation1)
  (define x :: int 6)
  (define x2 (* x 2))
  (+ x x2))

(define (constant-propagation2)
  (let ((cont2 (lambda (j::int) (+ 10 j))))
    (cont2 3)))

;; FIXME constant-folding is not done as well as we'd like.
;; Partly caused by setting dval=null in InlineCalls:visitReferenceExp.
;; The other problem is we visit a called Lambda (here cont2) before
;; visiting the argument (here i).  That also means we visit the
;; arguments without using the required parameter type.
(define (constant-propagation3)
  (let* ((i::int 2)
         (cont2 (lambda (j::int) (+ i j))))
    (cont2 i)))

(define (factorial-infer1 (x ::int))
  ;; The type of r should be inferred as integer.
  (define r 1)
  (do ((i ::int 1 (+ i 1)))
      ((> i x) r)
    (set! r (* r i))))

;; FUTURE - would like to infer type of r as integer
(define (factorial-infer2 (x ::int))
  (do ((i ::int 1 (+ i 1)) (r 1 (* r i)))
      ((> i x) r)))

(define (get-from-vector1 x::gnu.lists.FVector[java.lang.Integer] i::int)
  (x:get i))
(define (get-from-vector2 x::gnu.lists.FVector[java.lang.Integer] i::int)
  (x i))

(define (sum1 n::integer)
  (let loop ((i 0) (sum 0))
    (if (< i n)
        sum
        (loop (+ i 1) (+ i sum)))))

(define (sum2 n::double) ::double
  (let loop ((i 0.0d0) (sum 0))
    (if (< i n)
        sum
        (loop (+ i 1) (+ i sum)))))

(define (numcomp1 x y) ::int
  (if (< x y) 5 6))

(define(numcomp2 x y) ::int
  (let ((b (<= x y)))
    (if b 4 5)))

(define (numcomp3 x y z) ::int
  (if (> x y z) 3 2))

(define (numcomp4 x y z) ::int
  (if (> x 10 y 5 z) 6 3))

(define (numcomp5 x y z) ::int
  (let ((b (> x 10 y 5 z)))
    (if b 4 3)))

(define (eqv1 x y)
  (eqv? y x))

(define (raise1 x::int y)
  (if (< x 0) (raise y) (* x 2)))

(define (read1 p::input-port) ::int
  (let ((ch (read-char p)))
    (cond ((eof-object? ch) 1)
          ((char=? ch #\space) 2)
          ((and (char-ci>=? ch #\A) (char-ci<=? ch #\Z)) 3)
          (else 4))))

(define (handle-char ch::character)::void
  (format #t "{~w}" ch))
(define (string-for-each1 str::string)::void
  (string-for-each (lambda (x) (if (char>? x #\Space) (handle-char x))) str))
(define (string-for-each2 str::string)::void
  (string-for-each handle-char str))
(import (kawa string-cursors))
(define (string-for-each3 str::string)::void
  (string-cursor-for-each (lambda (x) (if (char>? x #\Space) (handle-char x)))
                          str))
(define (string-for-each4 str::string
                          start::string-cursor end::string-cursor)::void
  (string-cursor-for-each handle-char str start end))
(define (string-for-each5 str::string
                          start::int end::int)::void
  (srfi-13-string-for-each handle-char str start end))
(define (string-for-each6 str::string)::void
  (string-for-each
   (lambda (x y z) (handle-char x) (handle-char y) (handle-char z))
   str "BCDE" str))

(define (string-append1 (str::gnu.lists.FString) (ch::char))
  (string-append! str ch))

(define (string-append2 (str::gnu.lists.FString) (ch::character))
  (string-append! str ch))
(define (string-append3 (str::gnu.lists.FString) (ch::gnu.lists.FString))
  (string-append! str ch))
(define (string-append4 (str::gnu.lists.FString) (ch::gnu.text.Char))
  (string-append! str ch))
(define (string-append5 (str::gnu.lists.FString) (ch::java.lang.Character))
  (string-append! str ch))
(define (string-append6 (str::gnu.lists.FString) ch)
  (string-append! str ch))
(define (string-append7 (str::gnu.lists.FString) ch1 (ch2::character))
  (string-append! str ch1 ch2))

(define (translate-space-to-newline str::string)::string
  (let ((result (make-string 0)))
    (string-for-each
     (lambda (ch)
       (string-append! result
                       (if (char=? ch #\Space) #\Newline ch)))
     str)
    result))

(define (case01)
  (let ((key 5))
    (case key
      ((1 2 3 4) '1to4)
      ((5 6 7 8) '5to8))))

(define (case02)
  (let ((key (* 2 3)))
    (case key
      ((1 2 3 4) '1to4)
      ((5 6 7 8) '5to8))))

(define (case03)
  (let ((key 'five))
    (case key
      ((one two three four) '1to4)
      ((five six seven eight) '5to8))))

(define (case04 key)
  (case key
    ((1 2 3 4) (+ 5 (* 2 3)))
    ((5 6 7 8) (* 2 (+ 3 4)))
    (else (+ (* 3 2) 6))))

(define (case05 key::int)
  (case key
    ((1 2 3 4) (+ 5 (* 2 3)))
    ((5 6 7 8) (* 2 (+ 3 4)))
    (else (+ (* 3 2) 6))))

(define (case06 key::long)
  (case key
    ((1 2 3 4) (+ 5 (* 2 3)))
    ((5 6 7 8) (* 2 (+ 3 4)))
    (else (+ (* 3 2) 6))))

(define (case07 key)
  (case key
    ((1 2 3 4) 1)
    ((5 6 7 8) 2)
    (else 3)))

(define (case08 key::int)
  (case key
    ((1 2 3 4) 1)
    ((5 6 7 8) 2)
    (else 3)))

(define (case09 key::long)
  (case key
    ((1 2 3 4) 1)
    ((5 6 7 8) 2)
    (else 3)))

(define (case10 key)
  (case key
    ((1 2 3 4) '1to4)
    ((5 6 7 8) '5to8)
    (else 3)))

(define (case11 key::int)
  (case key
    ((1 2 3 4) '1to4)
    ((5 6 7 8) '5to8)
    (else 3)))

(define (case12 key::long)
  (case key
    ((1 2 3 4) '1to4)
    ((5 6 7 8) '5to8)
    (else 3)))

(define (case13 key::integer)
   (case key
     ((1 2 3 4) 1)
     ((5 6 7 8) 2)
     (else 3)))

(define (case14 key::char)
   (case key
     ((#\a #\b #\c #\d) 1)
     ((#\e #\f #\g #\h) 2)
     (else 3)))
