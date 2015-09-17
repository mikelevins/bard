(test-begin "java-array" 28)

(define obj1 ::Object (Object))
(define obj2 ::Object (Object))
(define obj-arr1 ::Object[] [obj1 #!null obj2])
(define obj-arr2 ::Object[] [obj1 #!null])

(define boolean-arr ::boolean[] [#t #f])
(define byte-arr ::byte[] [1 2 3])
(define char-arr ::char[] [#\1 #\2 #\3])
(define double-arr ::double[] [1 2 3])
(define float-arr ::float[] [1 2 3])
(define int-arr ::int[] [1 2 3])
(define long-arr ::long[] [1 2 3])
(define short-arr ::short[] [1 2 3])

;;; array vs non-array => #f
(test-equal #f (equal? obj-arr1 obj1))

;;; different array lengths => #f
(test-equal #f (equal? obj-arr1 obj-arr2))

;;; different array types => #f
(test-equal #f (equal? (Object[]) (byte[])))

(define str1 ::String "hello")
(define str2 ::string (string #\h #\e #\l #\l #\o))
(test-equal #f (equal? (Object[] str1) (String[] str1)))
(test-equal #f (equal? (String[] str1 #!null str2)
                       (string[] str2 #!null str1)))

;;; primitive arrays are equal if their values are equal
(test-equal (boolean[] #t #f) boolean-arr)
(let ((vals ::list '(1 2 3)))
  (test-equal (apply byte[] vals) byte-arr)
  (test-equal #f (equal? (apply char[] (map integer->char vals)) char-arr))
  (test-equal (apply double[] vals) double-arr)
  (test-equal (apply float[] vals) float-arr)
  (test-equal (apply int[] vals) int-arr)
  (test-equal (apply long[] vals) long-arr)
  (test-equal (apply short[] vals) short-arr))

(define bytes ::byte[] (byte[] length: 4))
(do ((i 0 (+ i 1)))
    ((= i bytes:length))
  (set! (bytes i) i))
(test-equal (byte[] 0 1 2 3) bytes)

;;; object arrays are equal iff all elements are equal
(test-equal (String[] str1 str2 #!null)
            (String[] str1 str2 #!null))
(test-equal #f (equal? (Object[] str1 #!null) (Object[] str1 str2)))
(test-equal (Object[] str1) (Object[] str2))

(test-equal (short[][] [1 2 3] [1 2 3]) (short[][] short-arr short-arr))

(test-equal #f (equal? (Object[] obj1) (Object[] obj2)))

(test-equal 5 (apply - (vector 10 4 1)))
(test-equal 5 (apply - (object[] 10 4 1)))
(test-equal 85 (apply - 100 (object[] 10 4 1)))
(test-equal 5 (apply - (integer[] 10 4 1)))
(test-equal 85 (apply - 100 (integer[] 10 4 1)))
(test-equal 5 (apply - (short[] 10 4 1)))
(test-equal 85 (apply - 100 (short[] 10 4 1)))
(test-equal 5.0d+0 (apply - (double[] 10 4 1)))
(test-equal 85.0d+0 (apply - 100 (double[] 10 4 1)))

(test-end)
