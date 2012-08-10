(include "error_macros.scm")

(define api:$BARD_UNDEFINED -1)
(define api:$BARD_NULL 0)
(define api:$BARD_CHARACTER 1)
(define api:$BARD_BOOLEAN 2)
(define api:$BARD_INTEGER 3)
(define api:$BARD_FLOAT 4)
(define api:$BARD_RATIO 5)
(define api:$BARD_SYMBOL 6)
(define api:$BARD_KEYWORD 7)
(define api:$BARD_TEXT 8)
(define api:$BARD_LIST 9)
(define api:$BARD_FRAME 10)
(define api:$BARD_PRIMITIVE 11)
(define api:$BARD_FUNCTION 12)
(define api:$BARD_METHOD 13)
(define api:$BARD_TYPE 14)
(define api:$BARD_UNRECOGNIZED 127)

(define api:$BARD_UNDEFINED_NAME "Undefined")
(define api:$BARD_NULL_NAME "Null")
(define api:$BARD_CHARACTER_NAME "Character")
(define api:$BARD_BOOLEAN_NAME "Boolean")
(define api:$BARD_INTEGER_NAME "Integer")
(define api:$BARD_FLOAT_NAME "Float")
(define api:$BARD_RATIO_NAME "Ratio")
(define api:$BARD_SYMBOL_NAME "Symbol")
(define api:$BARD_KEYWORD_NAME "Keyword")
(define api:$BARD_TEXT_NAME "Text")
(define api:$BARD_LIST_NAME "List")
(define api:$BARD_FRAME_NAME "Frame")
(define api:$BARD_PRIMITIVE_NAME "Primitive")
(define api:$BARD_FUNCTION_NAME "Function")
(define api:$BARD_METHOD_NAME "Method")
(define api:$BARD_TYPE_NAME "Type")
(define api:$BARD_UNRECOGNIZED_NAME "Unrecognized type")

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
   ((integer? obj) api:$BARD_INTEGER)
   ((##flonum? obj) api:$BARD_FLOAT)
   ((##ratnum? obj) api:$BARD_RATIO)
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
  (with-recorded-errors api:$BARD_UNRECOGNIZED_NAME
   (cond
   ((eqv? obj #!unbound) api:$BARD_UNDEFINED_NAME)
   ((null? obj) api:$BARD_NULL_NAME)
   ((char? obj) api:$BARD_CHARACTER_NAME)
   ((boolean? obj) api:$BARD_BOOLEAN_NAME)
   ((integer? obj) api:$BARD_INTEGER_NAME)
   ((##flonum? obj) api:$BARD_FLOAT_NAME)
   ((##ratnum? obj) api:$BARD_RATIO_NAME)
   ((symbol? obj) api:$BARD_SYMBOL_NAME)
   ((keyword? obj) api:$BARD_KEYWORD_NAME)
   ((string? obj) api:$BARD_TEXT_NAME)
   ((%primitive-method? obj) api:$BARD_PRIMITIVE_NAME)
   ((%function? obj) api:$BARD_FUNCTION_NAME)
   ((%method? obj) api:$BARD_METHOD_NAME)
   ((%type? obj) api:$BARD_TYPE_NAME)
   ((%frame? obj) api:$BARD_FRAME_NAME)
   ((%list? obj) api:$BARD_LIST_NAME)
   (else api:$BARD_UNRECOGNIZED_NAME))))

(define (cbard:read str)
  (with-recorded-errors #f
   (bard:read-from-string str)))

(define (cbard:read-lines str)
  (with-recorded-errors #f
    (call-with-input-string str
      (lambda (in)(read-all in read-line)))))

(define (cbard:read-nonempty-lines str)
  (let ((lines (cbard:read-lines str)))
    (if lines
        (filter nonempty-source-line? lines)
        '())))

(define (cbard:eval obj)
  (with-recorded-errors #f
   (%eval obj)))

(define (cbard:load-from-string str)
  (with-recorded-errors #f
    (%bard-load-from-string str)))

(define (cbard:print obj)
  (with-recorded-errors "#<Error printing an object>"
   (%as-string obj)))

(define (cbard:is-empty? obj)
  (cond
   ((null? obj) #t)
   ((list? obj) #f)
   ((%frame? obj)(<= (length (%frame-keys obj)) 0))
   ((string? obj)(<= (string-length obj) 0))
   ((vector? obj)(<= (vector-length obj) 0))
   (else #f)))

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



