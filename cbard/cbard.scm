(include "error_macros.scm")

(define api:$BARD_UNDEFINED -1)
(define api:$BARD_NULL 0)
(define api:$BARD_CHARACTER 1)
(define api:$BARD_BOOLEAN 2)
(define api:$BARD_NUMBER 3)
(define api:$BARD_SYMBOL 4)
(define api:$BARD_KEYWORD 5)
(define api:$BARD_TEXT 6)
(define api:$BARD_LIST 7)
(define api:$BARD_FRAME 8)
(define api:$BARD_PRIMITIVE 9)
(define api:$BARD_FUNCTION 10)
(define api:$BARD_METHOD 11)
(define api:$BARD_TYPE 12)
(define api:$BARD_UNRECOGNIZED 127)

(define (cbard:version)
  $bard-version-string)

(define (cbard:init-bard)
  (with-recorded-errors #f
   (%init-bard)
   #t))

(define (cbard:type-for-C obj)
  (with-recorded-errors api:$BARD_UNRECOGNIZED
   (cond
   ((eqv? obj #!unbound) api:$BARD_UNDEFINED)
   ((null? obj) api:$BARD_NULL)
   ((char? obj) api:$BARD_CHARACTER)
   ((boolean? obj) api:$BARD_BOOLEAN)
   ((number? obj) api:$BARD_NUMBER)
   ((symbol? obj) api:$BARD_SYMBOL)
   ((keyword? obj) api:$BARD_KEYWORD)
   ((string? obj) api:$BARD_TEXT)
   ((%list? obj) api:$BARD_LIST)
   ((%frame? obj) api:$BARD_FRAME)
   ((%primitive-method? obj) api:$BARD_PRIMITIVE)
   ((%function? obj) api:$BARD_FUNCTION)
   ((%method? obj) api:$BARD_METHOD)
   ((%type? obj) api:$BARD_TYPE)
   (else api:$BARD_UNRECOGNIZED))))

(define (cbard:typename obj)
  (with-recorded-errors "Unrecognized type"
   (cond
   ((eqv? obj #!unbound) "Undefined")
   ((null? obj) "Null")
   ((char? obj) "Character")
   ((boolean? obj) "Boolean")
   ((integer? obj) "Integer")
   ((##flonum? obj) "Float")
   ((##ratnum? obj) "Ratio")
   ((symbol? obj) "Symbol")
   ((keyword? obj) "Keyword")
   ((string? obj) "Text")
   ((%primitive-method? obj) "Primitive")
   ((%function? obj) "Function")
   ((%method? obj) "Method")
   ((%type? obj) "Type")
   ((%frame? obj) "Frame")
   ((%list? obj) "List")
   (else "Unrecognized type"))))

(define (cbard:read str)
  (with-recorded-errors #f
   (bard:read-from-string str)))

(define (cbard:eval obj)
  (with-recorded-errors #f
   (%eval obj)))

(define (cbard:print obj)
  (with-recorded-errors "#<Error printing an object>"
   (%as-string obj)))

(define (cbard:as-char obj)
  (if (char? obj)
      obj
      0))

(define (cbard:as-bool obj)
  (if obj #t #f))

(define (cbard:as-int obj)
  (if (number? obj)
      (inexact->exact (round obj))
      0))

(define (cbard:as-float obj)
  (if (number? obj)
      (exact->inexact obj)
      0.0))

(define (cbard:as-string obj)
  (with-recorded-errors "#<Error converting an object to a string>"
   (if (string? obj)
      obj
      (object->string obj))))


