;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-arithmetic.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of arithmetic primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite primitives-arithmetic-tests ()())

(addtest test-arithmetic-ops
  (ensure (bard::true? (bard::bard< 3 4)))
  (ensure (bard::false? (bard::bard< 4 4)))
  (ensure (bard::true? (bard::bard> 4 3)))
  (ensure (bard::false? (bard::bard> 4 4)))
  (ensure (bard::true? (bard::bard<= 4 4)))
  (ensure (bard::false? (bard::bard<= 6 4)))
  (ensure (bard::true? (bard::bard>= 4 3)))
  (ensure (bard::false? (bard::bard>= 1 4))))

(addtest test-equivalence-predicates
  (ensure (bard::true? (bard::identical? 3 3)))
  (ensure (bard::false? (bard::identical? 3 3.0)))
  (ensure (bard::true? (bard::identical? 'a 'a)))
  (ensure (bard::false? (bard::identical? "a" "a")))
  (ensure (bard::true? (bard::equal? (list "a") (list "a")))))


;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'primitives-arithmetic-tests))
