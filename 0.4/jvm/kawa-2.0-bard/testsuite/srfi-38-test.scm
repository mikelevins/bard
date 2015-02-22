(test-begin "srfi-38" 8)

(define circ1 (list 1 2 3))
(define circ2 (list 1 2 (list 'a 'b 'c) (vector "gsoc" "kawa") 3))
(define circ3 (cons 'a 'b))
(define a (cons 'a 'z))
(define b (cons 'b a))
(define c (cons 'c b))
(define circ4 (list c b a))
(define (long-line n) (if (= n 0) '() (cons 'a (long-line (- n 1)))))
(define edge1 (long-line 500))

(define *list* '(b c d e f g h i j k l m n o p q u r s t v w x y))
(define (long-shared res n)
  (if (= n 24)
      res
      (list (long-shared (cons (list-ref *list* (- n 1)) res)
                         (+ n 1))
            res)))
(define circ5 (long-shared (cons 'a 'z) 10))

(define (make-write-string form)
  (call-with-output-string
   (lambda (s) (write-shared form s))))

(set-car! (cdr circ1) circ1)
(test-equal "#0=(1 #0# 3)"
	    (make-write-string circ1))

(set-car! (cdr circ2) circ2)
(test-equal "#0=(1 #0# (a b c) #(\"gsoc\" \"kawa\") 3)"
	    (make-write-string circ2))

(set-car! (cddr (caddr circ2)) (caddr circ2))
(test-equal "#0=(1 #0# #1=(a b #1#) #(\"gsoc\" \"kawa\") 3)"
	    (make-write-string circ2))

(vector-set! (cadddr circ2) 1 (cadddr circ2))
(test-equal "#0=(1 #0# #1=(a b #1#) #2=#(\"gsoc\" #2#) 3)"
	    (make-write-string circ2))

(set-car! (cdr circ2) 2)
(test-equal "(1 2 #0=(a b #0#) #1=#(\"gsoc\" #1#) 3)"
	    (make-write-string circ2))

(set-cdr! circ3 circ3)
(test-equal "#0=(a . #0#)"
	    (make-write-string circ3))

(test-equal "((c . #0=(b . #1=(a . z)) #0# #1#)"
	    (make-write-string circ4))

;; Stress test.
(make-write-string edge1)

;; Testing both position markers > 9 and the hash table.
(test-equal
 "(((((((((((((((x . #0=(w . #1=(v . #2=(t . #3=(s . #4=(r . #5=(u . #6=(q . #7=(p . #8=(o . #9=(n . #10=(m . #11=(l . #12=(k . #13=(a . z)))))))))))))) #0#) #1#) #2#) #3#) #4#) #5#) #6#) #7#) #8#) #9#) #10#) #11#) #12#) #13#)"
  (make-write-string circ5))

(test-end)
