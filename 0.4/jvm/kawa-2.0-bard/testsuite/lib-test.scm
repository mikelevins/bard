;; -*- coding: utf-8 -*-

(test-begin "libs" 259)

(test-begin "vectors")
(test-equal '(dah dah didah)
            (vector->list '#(dah dah didah)))
(test-equal '(dah)
             (vector->list '#(dah dah didah) 1 2))
(test-equal #(dididit dah)
            (list->vector '(dididit dah)))

(test-equal #(#\A #\B #\C)
            (string->vector "ABC"))
(test-equal #(#\B #\C)
            (string->vector "ABCDE" 1 3))
(test-equal "123"
            (vector->string #(#\1 #\2 #\3)))
(test-equal "345"
            (vector->string #(#\1 #\2 #\3 #\4 #\5) 2))
(test-equal "34"
            (vector->string #(#\1 #\2 #\3 #\4 #\5) 2 4))
 
(let* ((a #(1 8 2 8))
       (b (vector-copy a)))
  (vector-set! b 0 3)
  (test-equal #(3 8 2 8) b)
  (test-equal #(8 2) (vector-copy b 1 3)))

(test-equal #(a b c d e f)
            (vector-append #(a b c) #(d e f)))

(let* ((a (vector 1 2 3 4 5))
       (b (vector 10 20 30 40 50)))
  (vector-copy! b 1 a 0 2)
  (test-equal #(10 1 2 40 50) b))

(test-equal #(1 2 smash smash 5)
            (let ()
              (define a (vector 1 2 3 4 5))
              (vector-fill! a 'smash 2 4)
              a))
(test-end)

(test-begin "bytevectors") ;; Some bytevector tests
(define bytes1 (bytevector #xCE #xBB))
(define lambda-char #\x3bb)
(define lambda-string (string lambda-char))
(test-equal #t (bytevector? bytes1))
(test-equal 2 (bytevector-length bytes1))
(test-equal 187 (bytevector-u8-ref bytes1 1))
(test-equal #f (bytevector? lambda-string))
(let ((bv (bytevector 1 2 3 4)))
  (bytevector-u8-set! bv 1 3)
  (test-equal #u8(1 3 3 4) bv))
(let ((a #u8(1 2 3 4 5)))
  (test-equal #u8(3 4) (bytevector-copy a 2 4)))
(let ((a (bytevector 1 2 3 4 5))
      (b (bytevector 10 20 30 40 50)))
  (bytevector-copy! b 1 a 0 2)
  (test-equal #u8(10 1 2 40 50) b))
(test-equal #u8(0 1 2 3 4 5)
            (bytevector-append #u8(0 1 2) #u8(3 4 5)))
(test-equal "A" (utf8->string #u8(#x41)))
(test-equal #u8(#xCE #xBB) (string->utf8 "λ"))
(test-equal bytes1 (string->utf8 lambda-string))
(test-equal lambda-string (utf8->string bytes1))
(test-end)

(import (srfi :2 and-let*))

(test-equal 1 (and-let* () 1))
(test-equal 2 (and-let* () 1 2))
(test-equal #t (and-let* ()))

(test-equal #f (let ((x #f)) (and-let* (x))))
(test-equal 1 (let ((x 1)) (and-let* (x))))
(test-equal #f (and-let* ((x #f)) ))
(test-equal 1  (and-let* ((x 1)) ))
(test-error (eval '(and-let* ( #f (x 1)))))
(test-equal #f (and-let* ( (#f) (x 1)) ))
(test-error (eval '(and-let* (2 (x 1)))))
(test-equal 1 (and-let* ( (2) (x 1)) ))
(test-equal 2 (and-let* ( (x 1) (2)) ))
(test-equal #f (let ((x #f)) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x) x)))
(test-equal "" (let ((x "")) (and-let* (x)  )))
(test-equal 2 (let ((x 1)) (and-let* (x) (+ x 1))))
(define xf #f)
(test-equal #f (and-let* (xf) (+ xf 1)))
(test-equal 2 (let ((x 1)) (and-let* (((positive? x))) (+ x 1))))
(test-equal #t (let ((x 1)) (and-let* (((positive? x))) )))
(test-equal #f (let ((x 0)) (and-let* (((positive? x))) (+ x 1))))
(test-equal 3 (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1))) (+ x 1))) )
;(must-be-a-syntax-error
;  (let ((x 1)) (and-let* (((positive? x)) (x (+ x 1)) (x (+ x 1))) (+ x 1)))
;)

(test-equal 2 (let ((x 1)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal 2 (let ((x 1)) (and-let* ( ((begin x)) ((positive? x))) (+ x 1))))
(test-equal #f (let ((x 0)) (and-let* (x ((positive? x))) (+ x 1))))
(test-equal #f (and-let* (xf ((positive? xf))) (+ xf 1)))
(test-equal #f (and-let* (((begin xf)) ((positive? xf))) (+ xf 1)))

(test-equal #f  (let ((x 1)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f  (let ((x 0)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))
(test-equal #f (and-let* (xf (y (- xf 1)) ((positive? y))) (/ xf y)))
(test-equal 3/2  (let ((x 3)) (and-let* (x (y (- x 1)) ((positive? y))) (/ x y))))

(define (symbol-parts s::symbol)
  (list (symbol-local-name s) (symbol-namespace-uri s) (symbol-prefix s)))

(test-equal '("loc1" "uri1" "pre")
            (symbol-parts (symbol "loc1" "uri1" "pre")))
(test-equal '("loc2" "uri2" "pre")
            (symbol-parts (symbol 'loc2 'uri2 'pre)))
(test-equal '("loc3" "uri3" "pre")
            (symbol-parts (apply symbol (list "loc3" "uri3" "pre"))))
(test-equal '("loc4" "uri4" "")
            (symbol-parts (symbol "loc4" "uri4")))
(test-equal '("loc5" "uri5" "pre")
            (symbol-parts (symbol "loc5" (namespace "uri5" "pre"))))
(test-equal '("abc:def" "" "")
	    (symbol-parts '|abc:def|))
(test-equal '("def" "" "abc")
	    (symbol-parts 'abc:def))

(require 'xml)

(test-equal '("abc" "URI" "")
	    (symbol-parts (element-name #<abc xmlns="URI"/>)))

;; Contributed by Helmut Eller.
(define (test-ev-req)
  (let* ((file (java.io.File:createTempFile "foo" ".scm"))
	 (filename (file:getAbsolutePath))
	 (now (lambda () (java.lang.System:currentTimeMillis)))
	 (cache-time (max gnu.expr.ModuleManager:LAST_MODIFIED_CACHE_TIME
			  1000))
	 (wait (lambda () (let* ((date (file:lastModified)))
			    (let loop ()
			      (when (< (- (now) date) (* 2 cache-time))
                                    (sleep 0.5))))))
         (make-form (lambda (bar-body)
                      &{
                        &|(module-export foo)
                        &|(module-static #t)
                        &|(module-compile-options
                        &|  warn-invoke-unknown-method: #t
                        &|  warn-undefined-variable: #t)
                        &|(define (foo) (bar))
                        &|(define (bar) &[bar-body])
                       }))
         (write-forms (lambda (bar-body)
			(wait)
			(call-with-output-file filename
			  (lambda (stream)
                            (write-string (make-form bar-body) stream)))
			(wait))))
    (try-finally
     (begin
       (write-forms &{"version 1"})
       (eval `(begin (require ,filename)
		     (define foo-1 foo)
		   (define result-1 (foo-1)))
	     (interaction-environment))
       (write-forms &{"version 2"})
       (eval `(begin (require ,filename)
		     (define result-2 (foo-1))
		     (list result-1 result-2))
	     (interaction-environment)))
     (delete-file filename))))
(test-equal
 '("version 1" "version 2")
 (test-ev-req))

(require 'syntax-utils)
(test-equal 'x (expand 'x))
(test-equal 1 (expand 1))
(test-equal '(let ((x 10)) x) (expand '(let ((x 10)) x)))
(test-equal '(lambda (x) x) (expand '(lambda (x) x)))
(test-equal '(if x 'a 'b) (expand '(if x 'a 'b)))
(test-equal '(set x 10) (expand '(set! x 10)))
(test-equal '(begin (x) (y)) (expand '(begin (x) (y))))
(test-equal "foo" (expand "foo"))
(test-equal '(quote (a b c)) (expand ''(a b c)))
(test-equal #f (expand '#f))
(test-equal #t (expand '#t))
(test-equal '(if (= x 1) (quote a) (if (= x 2) (quote b)))
      (expand '(cond ((= x 1) 'a)
			  ((= x 2) 'b))))
(test-equal '((let ((loop #!undefined)) 
	  (begin (set loop (lambda () (loop))) loop)))
      (expand '(let loop () (loop))))
(test-equal '(let ((x #!undefined)) (set x 10))
      (expand '(define x 10)))
(test-equal '(as <String> (quote a))
      (expand '(as String 'a)))

(test-equal '(lambda (a b c) #f)
	    (expand '(lambda (a b c) #f)))
(test-equal '(lambda (#!rest r) #f)
	    (expand '(lambda r #f)))
(test-equal '(lambda (#!rest r) #f)
	    (expand '(lambda (#!rest r) #f)))
(test-equal '(lambda (a b c #!rest r) #f)
	    (expand '(lambda (a b c #!rest r) #f)))
(test-equal '(lambda (#!optional d e f) #f)
	    (expand '(lambda (#!optional d e f) #f)))
(test-equal '(lambda (a b c #!optional d e f) #f)
	    (expand '(lambda (a b c #!optional d e f) #f)))
(test-equal '(lambda (a b c #!optional d e f #!rest r) #f)
	    (expand '(lambda (a b c #!optional d e f #!rest r) #f)))
(test-equal '(lambda (a b c #!rest r #!key d e f ) #f)
	    (expand '(lambda (a b c #!rest r #!key d e f ) #f)))
(test-equal '(lambda (a b c #!optional d e f #!rest r #!key g i j ) #f)
	    (expand '(lambda (a b c #!optional d e f #!rest r #!key g i j)
		      #f)))

(import (srfi :41 streams))

(define strm123
  (stream-cons 1
    (stream-cons 2
      (stream-cons 3
        stream-null))))
(test-equal 1 (stream-car strm123))
(test-equal 2 (stream-car (stream-cdr strm123)))
(test-equal #f
            (stream-pair?
             (stream-cdr
              (stream-cons (/ 1 0) stream-null))))
(test-equal #f (stream? (list 1 2 3)))
(test-equal 3 (stream-length strm123))

(define iter
  (stream-lambda (f x)
    (stream-cons x (iter f (f x)))))
(define nats (iter (lambda (x) (+ x 1)) 0))
(test-equal 1 (stream-car (stream-cdr nats)))

(define stream-add
  (stream-lambda (s1 s2)
    (stream-cons
      (+ (stream-car s1) (stream-car s2))
      (stream-add (stream-cdr s1)
                  (stream-cdr s2)))))
(define evens (stream-add nats nats))
(test-equal 0 (stream-car evens))
(test-equal 2 (stream-car (stream-cdr evens)))
(test-equal 4 (stream-car (stream-cdr (stream-cdr evens))))

(define (square x) (* x x))
(test-equal '(81 9) (stream->list (stream-map square (stream 9 3))))
(define (sigma f m n)
  (stream-fold + 0
    (stream-map f (stream-range m (+ n 1)))))
(test-equal 338350 (sigma square 1 100))

(test-equal '(1 2 3 2 1)
            (stream->list
             (stream-concat
              (stream
               (stream 1 2) (stream) (stream 3 2 1)))))
(test-equal
 '(0 1 4 9 16 25 36 49 64 81)
 (stream->list 10
               (stream-map (lambda (x) (* x x))
                           (stream-from 0))))
(test-equal '(3 4 3 4 3 4 3)
            (stream->list 7
             (stream-constant 3 4)))
(test-equal  '(1 3 5 7 9)
             (stream->list 5
                           (stream-filter odd? (stream-from 0))))

(test-equal '(0 4 16 36 64)
            (stream->list
             (stream-of (* x x)
                        (x in (stream-range 0 10))
                        (even? x))))
(test-equal '((1 1) (1 2) (2 1) (2 2) (3 1) (3 2))
            (stream->list
             (stream-of (list a b)
                        (a in (stream-range 1 4))
                        (b in (stream-range 1 3)))))
(test-equal '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4))
            (stream->list
             (stream-of (list i j)
                        (i in (stream-range 1 5))
                        (j in (stream-range (+ i 1) 5)))))

(define (stream-partition pred? strm)
  (stream-unfolds
    (lambda (s)
      (if (stream-null? s)
          (values s '() '())
          (let ((a (stream-car s))
                (d (stream-cdr s)))
            (if (pred? a)
                (values d (list a) #f)
                (values d #f (list a))))))
    strm))

(test-equal '((1 3 5) (2 4))
            (call-with-values
                (lambda ()
                  (stream-partition odd?
                                    (stream-range 1 6)))
              (lambda (odds evens)
                (list (stream->list odds)
                      (stream->list evens)))))

(define primes (let ()
                 (define-stream (next base mult strm)
                   (let ((first (stream-car strm))
                         (rest (stream-cdr strm)))
                     (cond ((< first mult)
                            (stream-cons first
                                         (next base mult rest)))
                           ((< mult first)
                            (next base (+ base mult) strm))
                           (else (next base
                                       (+ base mult) rest)))))
                 (define-stream (sift base strm)
                   (next base (+ base base) strm))
                 (define-stream (sieve strm)
                   (let ((first (stream-car strm))
                         (rest (stream-cdr strm)))
                     (stream-cons first
                                  (sieve (sift first rest)))))
                 (sieve (stream-from 2))))

(test-equal 997
            (stream-car
             (stream-reverse
              (stream-take-while
               (lambda (x) (< x 1000))
               primes))))

(define-stream (stream-finds eql? obj strm)
  (stream-of (car x)
    (x in (stream-zip (stream-from 0) strm))
    (eql? obj (cadr x))))
(define (stream-find eql? obj strm)
  (stream-car
    (stream-append
      (stream-finds eql? obj strm)
      (stream #f))))
(test-equal 2
            (stream-find char=? #\l
                         (list->stream
                          (string->list "hello"))))
(test-equal #f
            (stream-find char=? #\l
                         (list->stream
                          (string->list "goodbye"))))

(define power-table
  (stream-of
    (stream-of (expt m n)
      (m in (stream-from 1)))
      (n in (stream-from 2))))
(test-equal '(1 8 27 64 125 216 343 512 729 1000)
            (stream->list 10 (stream-ref power-table 1)))

(test-equal '(0 1 2 3 4)
            (stream-take 5 (stream-iterate (lambda (x) (+ x 1)) 0)))
(test-equal '(1 2 4 8 16)
            (stream-take 5 (stream-iterate (lambda (x) (* x 2)) 1)))

(test-equal
 '(1 2 3/2 5/3 8/5 13/8 21/13 34/21)
 (stream-take 8 (stream-iterate (lambda (x) (+ 1 (/ x))) 1)))

(test-equal '(0 1 3 6 10 15 )
            (stream-take 6 (stream-scan + 0 (stream-from 1))))

(define-stream (stream-merge lt? . strms)
  (define-stream (merge xx yy)
    (stream-match xx (() yy) ((x . xs)
      (stream-match yy (() xx) ((y . ys)
        (if (lt? y x)
            (stream-cons y (merge xx ys))
            (stream-cons x (merge xs yy))))))))
  (stream-let loop ((strms strms))
    (cond ((null? strms) stream-null)
          ((null? (cdr strms)) (car strms))
          (else (merge (car strms)
                       (apply stream-merge lt?
                         (cdr strms)))))))

(define-stream (stream-unique eql? strm)
  (if (stream-null? strm)
      stream-null
      (stream-cons (stream-car strm)
        (stream-unique eql?
          (stream-drop-while
            (lambda (x)
              (eql? (stream-car strm) x))
            strm)))))

(define (lsec proc . args)
  (lambda x (apply proc (append args x))))
(define hamming
  (stream-cons 1
    (stream-unique =
      (stream-merge <
        (stream-map (lsec * 2) hamming)
        (stream-map (lsec * 3) hamming)
        (stream-map (lsec * 5) hamming)))))
(test-equal '(1 2 3 4 5 6 8 9 10 12) (stream-take 10 hamming))

(test-begin "rnrs-lists" 50)

(import (rnrs lists))

(test-equal 4 (find even? '(3 1 4 1 5 9)))
(test-equal #f (find even? '(3 1 5 1 5 9)))
(test-equal #f (for-all even? '(3 1 4 1 5 9)))
(test-equal #f (for-all even? '(3 1 4 1 5 9 . 2)))
(test-equal #t (for-all even? '(2 4 14)))
(test-error (for-all even? '(2 4 14 . 9)))
(test-equal 14 (for-all (lambda (n) (and (even? n) n)) '(2 4 14)))
(test-equal #t (for-all < '(1 2 3) '(2 3 4)))
(test-equal #f (for-all < '(1 2 4) '(2 3 4)))
(test-equal #t (exists even?  '(3 1 4 1 5 9)))
(test-equal #f (exists even?  '(3 1 1 5 9)))
(test-error (exists even?  '(3 1 1 5 9 . 2)))
(test-equal 2 (exists (lambda (n) (and (even? n) n)) '(2 1 4 14)))
(test-equal #t (exists < '(1 2 4) '(2 3 4)))
(test-equal #f (exists > '(1 2 3) '(2 3 4)))
(test-equal '(4 2 6) (filter even? '(3 1 4 1 5 9 2 6)))
(test-equal '((4 2 6) (3 1 1 5 9))
            (call-with-values
                (lambda ()
                  (partition even? '(3 1 4 1 5 9 2 6)))
              (lambda (evens odds)
                (list evens odds))))
(test-equal 15 (fold-left + 0 '(1 2 3 4 5)))
(test-equal '(5 4 3 2 1) (fold-left (lambda (a e) (cons e a)) '()
                                    '(1 2 3 4 5)))
(test-equal 7 (fold-left (lambda (count x)
                           (if (odd? x) (+ count 1) count))
                         0 '(3 1 4 1 5 9 2 6 5 3)))
(test-equal 7 (fold-left (lambda (max-len s)
                           (max max-len (string-length s)))
                         0 '("longest" "long" "longer")))
(test-equal '((((q) . a) . b) . c) (fold-left cons '(q) '(a b c)))
(test-equal 21 (fold-left + 0 '(1 2 3) '(4 5 6)))
(test-equal 15 (fold-right + 0 '(1 2 3 4 5)))
(test-equal '(1 2 3 4 5) (fold-right cons '() '(1 2 3 4 5)))
(test-equal '(3 1 1 5 9 5) (fold-right (lambda (x l)
                                         (if (odd? x) (cons x l) l))
                                       '() '(3 1 4 1 5 9 2 6 5)))
(test-equal '(a b c q) (fold-right cons '(q) '(a b c)))
(test-equal 21 (fold-right + 0 '(1 2 3) '(4 5 6)))
(test-equal '(3 1 1 5 9 5) (remp even? '(3 1 4 1 5 9 2 6 5)))
(test-equal '(3 4 5 9 2 6 5) (remove 1 '(3 1 4 1 5 9 2 6 5)))
(test-equal '(3 4 5 9 2 6 5) (remv 1 '(3 1 4 1 5 9 2 6 5)))
(test-equal '(bar baz) (remq 'foo '(bar foo baz)))
(test-equal '(4 1 5 9 2 6 5) (memp even? '(3 1 4 1 5 9 2 6 5)))
(test-equal '(a b c) (memq 'a '(a b c)))
(test-equal '(b c) (memq 'b '(a b c)))
(test-equal #f (memq 'a '(b c d)))
(test-equal #f (memq (list 'a) '(b (a) c)))
(test-equal '((a) c) (member (list 'a) '(b (a) c)))
;; (test-equal '(101 102) (memq 101 '(100 101 102))) ; result unspecified
(test-equal '(101 102) (memv 101 '(100 101 102)))

(define d '((3 a) (1 b) (4 c)))
(test-equal '(4 c) (assp even? d))
(test-equal '(3 a) (assp odd? d))

(define e '((a 1) (b 2) (c 3)))
(test-equal '(a 1) (assq 'a e))
(test-equal '(b 2) (assq 'b e))
(test-equal #f (assq 'd e))

(test-equal #f (assq (list 'a) '(((a)) ((b)) ((c)))))
(test-equal '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
;; (test-equal '(5 7) (assq 5 '((2 3) (5 7) (11 13)))) ; result unspecified
(test-equal '(5 7) (assv 5 '((2 3) (5 7) (11 13))))

(test-equal '(1 2 3 4 5) (cons* 1 2 '(3 4 5)))
(test-equal '(1 2 . 3) (cons* 1 2 3))
(test-equal 1 (cons* 1))

(test-end "rnrs-lists")

(test-begin "strings")
(import (kawa string-cursors))

(define str1 "a😂b😼c")
(define sc1e::string-cursor (string-cursor-end str1))
(define str1lst '())
(do ((sc::string-cursor (string-cursor-start str1)
                        (string-cursor-next str1 sc)))
    ((string-cursor>=? sc sc1e))
  (set! str1lst (cons (as int (string-cursor-ref str1 sc)) str1lst)))
(test-equal '(97 128514 98 128572 99) (reverse str1lst))

(define str2lst '())
(string-cursor-for-each
 (lambda (x) (set! str2lst (cons (char->integer x) str2lst)))
 str1 (as string-cursor 3))
(test-equal '(98 128572 99) (reverse str2lst))
;; FIXME more

(test-end)

(test-begin "char-sets" 91)

(import (srfi :14 char-sets))
(import (rnrs sorting))

; char-set=
(test-equal #t (char-set=))
(test-equal #t (char-set= char-set:full))
(test-equal #t (char-set= char-set:full char-set:full))
(test-equal #f (char-set= char-set:empty char-set:full))
(test-equal #t (char-set= char-set:empty (char-set)))
; char-set<=
(test-equal #t (char-set<=))
(test-equal #t (char-set<= char-set:empty))
(test-equal #t (char-set<= char-set:empty char-set:full))
(test-equal #t (char-set<= char-set:empty char-set:lower-case
                           char-set:full))
(test-equal #t (char-set<= (char-set #\u) (char-set #\u)))
(test-equal #t (char-set<= (char-set #\u) (char-set #\u #\a)))
; char-set-hash
(test-equal #t (= (char-set-hash char-set:empty)
                  (char-set-hash (char-set))))
(test-equal #t (<= 0 (char-set-hash char-set:lower-case 50) 49))
; char-set-cursor, char-set-ref, char-set-cursor-next,
; end-of-char-set?
(define cs (char-set #\H #\e #\l #\l #\o #\, #\W #\o #\r #\l #\d))
(test-equal '(#\, #\H #\W #\d #\e #\l #\o #\r)
            (list-sort char<?
                       (let lp ((cur (char-set-cursor cs)) (ans '()))
                         (if (end-of-char-set? cur) ans
                             (lp (char-set-cursor-next cs cur)
                                 (cons (char-set-ref cs cur) ans))))))
(test-equal #t (end-of-char-set? (char-set-cursor char-set:empty)))
; char-set-fold
(test-equal '(#\, #\H #\W #\d #\e #\l #\o #\r)
            (list-sort char<? (char-set-fold cons '() cs)))
(test-equal 0 (char-set-fold (lambda (c i) (+ i 1)) 0 char-set:empty))
(test-equal 128 (char-set-fold (lambda (c i) (+ i 1)) 0 char-set:ascii))
; char-set-unfold, char-set-unfold!
(define abc (char-set #\a #\b #\c))
(test-equal #t (char-set= abc (char-set-unfold
                               null? car cdr '(#\a #\b #\c))))
(test-equal #t (char-set= abc (char-set-unfold
                               null? car cdr '(#\a #\c)
                               (char-set #\b))))
(test-equal #t (char-set= abc (char-set-unfold!
                               null? car cdr '(#\a #\c)
                               (char-set #\b))))
; also testing the definition of char-set:full
(test-equal #t (char-set= char-set:full
                          (char-set-unfold
                           (lambda (i) (> i #x10FFFF))
                           integer->char
                           (lambda (i) (+ i 1))
                           0)))
; char-set-for-each is only useful for side-effects, so no test
; provided
; char-set-map
(test-equal #t (char-set= abc (char-set-map char-downcase
                                            (char-set #\A #\B #\C))))
; char-set-copy
(test-equal #t (equal? char-set:empty (char-set-copy char-set:empty)))
(test-equal '(#\, #\H #\W #\d #\e #\l #\o #\r)
            (list-sort char<? (char-set-fold cons '()
                                             (char-set-copy cs))))
; list->char-set, list->char-set!
(test-equal #t (char-set= abc (list->char-set '(#\a #\b #\c))))
(test-equal #t (char-set= abc (list->char-set '(#\a)
                                              (char-set #\b #\c))))
(test-equal #t (char-set= abc (list->char-set! '(#\a)
                                               (char-set #\b #\c))))
; string->char-set, string->char-set!
(test-equal #t (char-set= abc (string->char-set "abc")))
(test-equal #t (char-set= abc (string->char-set "ab"
                                                (char-set #\c))))
(test-equal #t (char-set= abc (string->char-set! "ab"
                                                 (char-set #\c))))
; char-set-filter and the meanings of some standard character sets
(test-equal #t (char-set=
                char-set:empty
                (char-set-filter (lambda (c) #f) char-set:full)))
(test-equal #t (char-set=
                char-set:ascii
                (char-set-filter
                 (lambda (c) (> 128 (char->integer c)))
                 char-set:full)))
(test-equal #t (char-set=
                char-set:iso-control
                (char-set-filter
                 (lambda (c) (java.lang.Character:isISOControl
                              (char->integer c))) char-set:full)))
(test-equal #t (char-set=
                char-set:title-case
                (char-set-filter
                 (lambda (c) (java.lang.Character:title-case?
                              (char->integer c))) char-set:full)))

; Some of these tests only succeed on Java 7 (or later), which supports
; Unicode 6. On earlier Javas, the java.lang.Character predicates will
; disagree with the char-set definitions.
(define-syntax expect-fail-unless-unicode-6
  (syntax-rules ()
    ((expect-fail-unless-unicode-6 count)
     (cond-expand ((or java-7 class-exists:java.util.concurrent.TransferQueue))
                  (else (test-expect-fail count))))))
(expect-fail-unless-unicode-6 8)
(test-equal #t (char-set=               ; only on Java 7
                char-set:lower-case
                (char-set-filter
                 (lambda (c) (java.lang.Character:lower-case?
                              (char->integer c))) char-set:full)))
(test-equal #t (char-set=               ; only on Java 7
                char-set:upper-case
                (char-set-filter
                 (lambda (c) (java.lang.Character:upper-case?
                              (char->integer c))) char-set:full)))
(test-equal #t (char-set=               ; only on Java 7
                char-set:letter
                (char-set-filter
                 (lambda (c) (java.lang.Character:letter?
                              (char->integer c))) char-set:full)))
(test-equal #t (char-set=               ; only on Java 7
                char-set:digit
                (char-set-filter
                 (lambda (c) (java.lang.Character:digit?
                              (char->integer c))) char-set:full)))
(test-equal                             ; only on Java 7
 #t (char-set=
     char-set:punctuation
     (char-set-filter
      (lambda (c)
        (let ((type ::byte (java.lang.Character:get-type
                            (char->integer c))))
          (or (= type java.lang.Character:CONNECTOR_PUNCTUATION)
              (= type java.lang.Character:DASH_PUNCTUATION)
              (= type java.lang.Character:START_PUNCTUATION)
              (= type java.lang.Character:END_PUNCTUATION)
              (= type java.lang.Character:INITIAL_QUOTE_PUNCTUATION)
              (= type java.lang.Character:FINAL_QUOTE_PUNCTUATION)
              (= type java.lang.Character:OTHER_PUNCTUATION))))
      char-set:full)))
(test-equal                             ; only on Java 7
 #t (char-set=
     char-set:symbol
     (char-set-filter
      (lambda (c)
        (let ((type ::byte (java.lang.Character:get-type
                            (char->integer c))))
          (or (= type java.lang.Character:MATH_SYMBOL)
              (= type java.lang.Character:CURRENCY_SYMBOL)
              (= type java.lang.Character:MODIFIER_SYMBOL)
              (= type java.lang.Character:OTHER_SYMBOL))))
      char-set:full)))
(test-equal                             ; only on Java 7
 #t (char-set=
     char-set:whitespace
     (char-set-filter
      (lambda (c)
        (or (char=? c #\u0009)
            (char=? c #\u000a)
            (char=? c #\u000b)
            (char=? c #\u000c)
            (char=? c #\u000d)
            (let ((type ::byte (java.lang.Character:get-type
                                (char->integer c))))
              (or (= type java.lang.Character:SPACE_SEPARATOR)
                  (= type java.lang.Character:LINE_SEPARATOR)
                  (= type java.lang.Character:PARAGRAPH_SEPARATOR)))))
      char-set:full)))
(test-equal                             ; only on Java 7
 #t (char-set=
     char-set:blank
     (char-set-filter
      (lambda (c)
        (or (char=? c #\u0009)
            (let ((type ::byte (java.lang.Character:get-type
                                (char->integer c))))
              (= type java.lang.Character:SPACE_SEPARATOR))))
      char-set:full)))


; char-set-filter!
(test-equal #t (char-set= (char-set #\a #\b #\c)
                          (char-set-filter!
                           char-lower-case?
                           (char-set #\b #\c #\D #\E #\F)
                           (char-set #\a))))
; ucs-range->char-set, ucs-range->char-set!
(test-equal #t (char-set= char-set:ascii (ucs-range->char-set 0 128)))
(test-equal
 #t (char-set= char-set:full
               (ucs-range->char-set! 100 (+ 1 #x10FFFF) #f
                                     (ucs-range->char-set 0 100))))
; ->char-set
(define csa ::char-set (char-set #\a))
(test-equal #t (char-set= csa (->char-set (char-set #\a))))
(test-equal #t (char-set= csa (->char-set #\a)))
(test-equal #t (char-set= csa (->char-set "a")))
; char-set->list, char-set->string -- order is not guaranteed
(test-equal '(#\a #\b #\c) (list-sort char<? (char-set->list abc)))
(test-equal "a" (char-set->string csa))
(test-equal #t (let ((s ::String (char-set->string abc)))
                 (or (string=? s "abc") (string=? s "acb")
                     (string=? s "bac") (string=? s "bca")
                     (string=? s "cab") (string=? s "cba"))))
; char-set-size, char-set-count
(test-equal 1 (char-set-size csa))
(test-equal 0 (char-set-size char-set:empty))
(test-equal 3 (char-set-size abc))
(test-equal 2 (char-set-size (char-set #\A #\z)))
(test-equal (char-set-size char-set:letter)
            (char-set-count (lambda (c) #t) char-set:letter))
(test-equal 1 (char-set-count char-lower-case? (char-set #\A #\z)))
; char-set-contains?, char-set-every, char-set-any
(test-equal #f (char-set-contains? char-set:upper-case #\a))

(test-equal #t (char-set-contains?
                char-set:full
                (integer->char (remainder
                                (abs ((java.util.Random):nextInt))
                                (+ 1 #x10FFFF)))))

(test-equal #f (char-set-any char-upper-case? char-set:lower-case))

(expect-fail-unless-unicode-6 1)
(test-equal #t (char-set-every char-upper-case? char-set:upper-case))
; char-set-adjoin, char-set-adjoin!
(test-equal #t (char-set= abc
                          (char-set-adjoin (char-set #\a) #\b #\c)))
(test-equal #t (char-set= abc
                          (char-set-adjoin! (char-set #\a) #\b #\c)))
; char-set-delete, char-set-delete!
(test-equal #t (char-set=
                abc
                (char-set-delete (string->char-set "fdbaec")
                                 #\d #\e #\f)))
; char-set-complement, char-set-complement!
(test-equal #t (char-set= char-set:full
                          (char-set-complement char-set:empty)))
(test-equal #t (char-set= char-set:empty
                          (char-set-complement! (char-set-complement
                                                 char-set:empty))))
; char-set-union, char-set-union!, meanings of standard derived sets
(test-equal #t (char-set= char-set:empty (char-set-union)))
(test-equal #t (char-set= abc (char-set-union abc)))
(test-equal #t (char-set= abc (char-set-union abc abc)))
(test-equal #t (char-set= char-set:letter+digit
                          (char-set-union char-set:letter
                                          char-set:digit)))
(test-equal #t (char-set= char-set:graphic
                          (char-set-union char-set:letter+digit
                                          char-set:punctuation
                                          char-set:symbol)))
(test-equal #t (char-set= char-set:printing
                          (char-set-union char-set:graphic
                                          char-set:whitespace)))
(test-equal #t (char-set= abc (char-set-union! (char-set #\a)
                                               (char-set #\b)
                                               (char-set #\c))))
(test-equal
 #t
 (char-set= char-set:full
            (char-set-union char-set:letter
                            (char-set-complement char-set:letter))))
; char-set-intersection, char-set-intersection!
(test-equal #t (char-set= char-set:full (char-set-intersection)))
(test-equal #t (char-set= char-set:graphic
                          (char-set-intersection char-set:graphic)))
(test-equal
 #t
 (char-set= char-set:empty
            (char-set-intersection abc (char-set-complement abc))))
(test-equal
 #t
 (char-set= (string->char-set "aeiou")
            (char-set-intersection
             char-set:lower-case
             (string->char-set "abcdefghijklmnopqrstuvwxyz")
             (string->char-set
              "oNLY VoWeLS aRe LoWeR CaSe iN THiS, You See?"))))
(test-equal
 #t
 (char-set= (string->char-set "aeiou")
            (char-set-intersection!
             (string->char-set "abcdefghijklmnopqrstuvwxyz")
             (string->char-set
              "oNLY VoWeLS aRe LoWeR CaSe iN THiS, You See?"))))
; char-set-difference, char-set-difference!
(test-equal
 #t
 (char-set= abc
            (char-set-difference (string->char-set "abcde")
                                 (string->char-set "de"))))
(test-equal
 #t
 (char-set= abc
            (char-set-difference! (string->char-set "abcde")
                                  (string->char-set "de"))))
(test-equal
 #t
 (char-set= char-set:letter
            (char-set-difference char-set:letter+digit
                                 char-set:digit)))
; char-set-xor, char-set-xor!
(test-equal #t (char-set= char-set:empty (char-set-xor)))
(test-equal
 #t (char-set= abc (char-set-xor (char-set #\a)
                                 (char-set #\b)
                                 (char-set #\c))))
(test-equal
 #t (char-set= abc (char-set-xor! (char-set #\a)
                                  (char-set #\b)
                                  (char-set #\c))))
(test-equal #t (char-set= abc (char-set-xor abc abc abc)))
(test-equal #t (char-set= (string->char-set "adz")
                          (char-set-xor! (string->char-set "abcd")
                                         (string->char-set "bc")
                                         (char-set #\z))))
(test-equal #t (char-set= (string->char-set "abde")
                          (char-set-xor abc
                                        (string->char-set "cde"))))
; char-set-diff+intersection, char-set-diff+intersection!
(test-equal '((#\a) (#\b #\c))
            (call-with-values
                (lambda ()
                  (char-set-diff+intersection
                   (string->char-set "abc") (string->char-set "bc")))
              (lambda (diff intersection)
                (list (list-sort char<? (char-set->list diff))
                      (list-sort char<? (char-set->list
                                         intersection))))))
(test-equal '((#\a) (#\b #\c))
            (call-with-values
                (lambda ()
                  (char-set-diff+intersection!
                   (string->char-set "abc") (string->char-set "bc")))
              (lambda (diff intersection)
                (list (list-sort char<? (char-set->list diff))
                      (list-sort char<? (char-set->list
                                         intersection))))))
(test-end)

(test-end)
