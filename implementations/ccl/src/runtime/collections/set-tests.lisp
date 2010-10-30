;;; ---------------------------------------------------------------------
;;; SETS test suite
;;; ---------------------------------------------------------------------

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.SETS.TEST"
  (:use :cl :as :fn :lift))

(in-package :folio.collections.sets.test)

;;; ---------------------------------------------------------------------
;;; common suite class
;;; ---------------------------------------------------------------------

(deftestsuite set-tests () ())

;;; ---------------------------------------------------------------------
;;;  set AS tests
;;; ---------------------------------------------------------------------

(deftestsuite set-as-tests (set-tests) ())

(addtest (set-as-tests)
  test-fset-set-to-fset-set
  (let ((s (set:make 0 1 2 3)))
    (ensure-same :equal
                 (fset:compare s (as 'fset:set s)))))

(addtest (set-as-tests)
  test-fset-seq-to-fset-set
  (let* ((s1 (seq:make 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-fset-seq
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'fset:seq s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-list-to-fset-set
  (let* ((s1 (list 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-list
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'list s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-vector-to-fset-set
  (let* ((s1 (vector 0 1 2 3))
         (s2 (as 'fset:set s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-vector
  (let* ((s1 (set:make 0 1 2 3))
         (s2 (as 'vector s1)))
    (ensure
     (every (fn (x) (eql x :equal))
            (as 'list
                (seq:image (fn (u v) (fset:compare u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-string-to-fset-set
  (let* ((s1 "A test string")
         (s2 (as 'fset:set s1)))
    (ensure
     (every 'identity 
            (as 'list
                (seq:image (fn (u v) (char= u v))
                           s1 s2))))))

(addtest (set-as-tests)
  test-fset-set-to-string
  (let* ((s1 "AbCd")
         (s2 (set:make #\A #\b #\C #\d)))
    (ensure
     (every (fn (ch) (member ch (as 'list s1) :test 'char=)) 
            (as 'list (as 'string s2))))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'set-as-tests)

;;; ---------------------------------------------------------------------
;;;  set API tests
;;; ---------------------------------------------------------------------

(deftestsuite set-api-tests (set-tests) ())

(addtest (set-api-tests)
  test-adjoin
  (ensure (set:contains? (set:adjoin 12345 '(0 1 2 3))
                     12345))
  (ensure (set:contains? (set:adjoin 12345 (set:make 0 1 2 3))
                     12345)))

(addtest (set-api-tests)
  test-contains?
  (ensure (set:contains? (set:make 0 1 2 3) 3))
  (ensure (set:contains? (list 0 1 2 3) 3))
  (ensure (not (set:contains? (set:make 0 1 2 3) 5)))
  (ensure (not (set:contains? (list 0 1 2 3) 5))))

(addtest (set-api-tests)
  test-difference
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 3 4 5))
        (expected (list 0 1 2)))
    (ensure (eql :equal (fset:compare (as 'fset:set expected)
                                      (set:difference (as 'fset:set s1)
                                                      (as 'fset:set s2)))))
    (ensure (null (set:difference expected (set:difference s1 s2))))))

(addtest (set-api-tests)
  test-intersection
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 3 4 5 6 7 8))
        (expected (list 3 4 5)))
    (ensure (eql :equal (fset:compare (as 'fset:set expected)
                                      (set:intersection (as 'fset:set s1)
                                                      (as 'fset:set s2)))))
    (ensure (null (set:difference expected (set:intersection s1 s2))))))

(addtest (set-api-tests)
  test-subset?
  (let ((s1 (list 0 1 2 3 4 5))
        (s2 (list 3 4 5)))
    (ensure (set:subset? s2 s1))
    (ensure (set:subset? (as 'fset:set s2) (as 'fset:set s1)))))

(addtest (set-api-tests)
  test-union
  (let ((s1 (list 0 1 2))
        (s2 (list 3 4 5))
        (expected (list 0 1 2 3 4 5)))
    (ensure (null (set:difference expected (set:union s1 s2))))
    (ensure (null (set:difference (set:union s1 s2) expected)))
    (ensure (eql :equal (fset:compare (as 'fset:set expected)
                                      (set:union (as 'fset:set s1) (as 'fset:set s2)))))))

;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'set-api-tests)

;;; ---------------------------------------------------------------------
;;; run all tests
;;; ---------------------------------------------------------------------
;;; (setf *TEST-DESCRIBE-IF-NOT-SUCCESSFUL?* t)
;;; (lift:run-tests :suite 'set-tests)



