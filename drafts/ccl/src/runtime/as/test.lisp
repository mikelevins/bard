;;; ---------------------------------------------------------------------
;;; AS test suite
;;; ---------------------------------------------------------------------

(in-package :cl-user)

(defpackage "FOLIO.AS.TEST"
  (:use :cl :as :lift))

(in-package :folio.as.test)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite as-tests () ())

;;; ---------------------------------------------------------------------
;;; test class for unknown conversions
;;; ---------------------------------------------------------------------

(defclass worthless-testing-class () ())

;;; ---------------------------------------------------------------------
;;;  test the to-number converters
;;; ---------------------------------------------------------------------

(deftestsuite as-number-tests (as-tests) ())

(addtest (as-number-tests)
  test-unknown-number-conversion
  (ensure-error 
    (as 'worthless-testing-class 5)))

(addtest (as-number-tests)
  test-character->number
  (ensure (numberp (as 'number #\A))))

(addtest (as-number-tests)
  test-integer->float
  (ensure (= 5.0 (as 'float 5))))

(addtest (as-number-tests)
  test-integer->integer
  (let ((i 1234567890))
    (ensure (= i (as 'integer i)))))

(addtest (as-number-tests)
  test-float->integer
  (let ((f 23.45))
    (multiple-value-bind (i r) (as 'integer f)
      (ensure (= i 23))
      ;; floating point is not so very precise...
      (ensure (< 0.44 r 0.46)))))

(addtest (as-number-tests)
  test-ratio->integer
  (let ((rat 27/23))
    (multiple-value-bind (i r) (as 'integer rat)
      (ensure (= i 1))
      ;; floating point is not so very precise...
      (ensure (= r 4/23)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-number-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-character converters
;;; ---------------------------------------------------------------------

(deftestsuite as-character-tests (as-tests) ())

(addtest (as-character-tests)
  test-integer-to-character
  (ensure (characterp (as 'character 71))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-character-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-symbol converters
;;; ---------------------------------------------------------------------

(deftestsuite as-symbol-tests (as-tests) ())

(addtest (as-symbol-tests)
  test-string-to-symbol
  (let* ((nm "Foo")
         (s (as 'symbol nm)))
    (ensure (equal nm (symbol-name s)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-symbol-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-string converters
;;; ---------------------------------------------------------------------

(deftestsuite as-string-tests (as-tests) ())

(addtest (as-string-tests)
  test-unkown-string-conversion
  (ensure (stringp (as 'string (make-instance 'worthless-testing-class) ))))

(addtest (as-string-tests)
  test-string-to-string
  (let ((s "This is a random string suitable for random tests"))
    (ensure (string= s (as 'string s)))))

(addtest (as-string-tests)
  test-symbol-to-string
  (let ((s '|Foo Bar|))
    (ensure (eq s (read-from-string (as 'string s))))))

(addtest (as-string-tests)
  test-gensym-to-string
  (let ((s (gensym)))
    (ensure (string= (symbol-name s) 
                     (symbol-name (read-from-string (as 'string s)))))))

(addtest (as-string-tests)
  test-character-to-string
  (let* ((chars (mapcar (lambda (i) (as 'character i))
                        (loop for i from 0 upto 255 collect i)))
         (ch (elt chars (random 256))))
    (ensure (char= ch (elt (as 'string ch) 0)))))

(addtest (as-string-tests)
  test-char-vector-to-string
  (let* ((s "A random test string")
         (chars (coerce s 'vector)))
    (ensure (string= s (as 'string chars)))))

(addtest (as-string-tests)
  test-vector-to-string
  (let* ((v #(0 1 2 3 4 5))
         (s "#(0 1 2 3 4 5)"))
    (ensure (string= s (as 'string v)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-string-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-vector converters
;;; ---------------------------------------------------------------------

(deftestsuite as-vector-tests (as-tests) ())

(addtest (as-vector-tests)
  test-unkown-vector-conversion
  (ensure-error (as 'vector (make-instance 'worthless-testing-class) )))

(addtest (as-vector-tests)
  test-vector-to-vector
  (let ((v #(0 4 'foo "Bar" '(baz))))
    (ensure (equalp v (as 'vector v)))))

(addtest (as-vector-tests)
  test-list-to-vector
  (let ((v (list 0 4 'foo "Bar" '(baz))))
    (ensure (every 'equal v (as 'vector v)))))

(addtest (as-vector-tests)
  test-string-to-vector
  (let ((s "Another random string"))
    (ensure (every 'char= s (as 'vector s)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-vector-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-list converters
;;; ---------------------------------------------------------------------

(deftestsuite as-list-tests (as-tests) ())

(addtest (as-list-tests)
  test-unkown-list-conversion
  (ensure-error (as 'list (make-instance 'worthless-testing-class) )))

(addtest (as-list-tests)
  test-list-to-list
  (let ((l (list 0 4 'foo "Bar" '(baz))))
    (ensure (equalp l (as 'list l)))))

(addtest (as-list-tests)
  test-vector-to-list
  (let ((v #(0 4 'foo "Bar" '(baz))))
    (ensure (every 'equal v (as 'list v)))))

(addtest (as-list-tests)
  test-string-to-list
  (let ((s "Another random string"))
    (ensure (every 'char= s (as 'list s)))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-list-tests)

;;; ---------------------------------------------------------------------
;;;  test the to-class converter
;;; ---------------------------------------------------------------------

(deftestsuite as-class-tests (as-tests) ())

(addtest (as-class-tests)
  test-symbol-to-class
  (ensure (eql (find-class 'cl:standard-class)
               (as 'class 'cl:standard-class))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-class-tests)

;;; ---------------------------------------------------------------------
;;;  run all the tests
;;; ---------------------------------------------------------------------

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'as-tests)
