;; From Savannah bug #36863 "then clause grows stack with no else clause"
(define (first (iterable java.lang.Iterable))
  (let ((f (lambda (x) #t))
	(iter (iterable:iterator)))
    (let loop ()
      (if (iter:hasNext)
	  (let ((item (iter:next)))
	    (if (f item)
		item
		(loop)))
	  #f))))

;; These are variations, with different bodies for the function f.
(define (no-match (iterable java.lang.Iterable))
  (let ((f (lambda (x) #f))
	(iter (iterable:iterator)))
    (let loop ()
      (if (iter:hasNext)
	  (let ((item (iter:next)))
	    (if (f item)
		item
		(loop)))
	  #f))))

(define (odd-match (iterable java.lang.Iterable))
  (let ((f (lambda (x) (odd? x)))
	(iter (iterable:iterator)))
    (let loop ()
      (if (iter:hasNext)
	  (let ((item (iter:next)))
	    (if (f item)
		item
		(loop)))
	  #f))))

(format #t "~s.~%" (first (vector 3 4 5)))
;; Output: 3.
(format #t "~s.~%" (no-match (vector 3 4 5)))
;; Output: #f.
(format #t "~s.~%" (odd-match (vector 2 4 7 9)))
;; Output: 7.
