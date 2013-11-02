;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of general-purpose utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite util-tests ()())

;;; length=1? 
(addtest test-length=1? 
  (ensure (bard::length=1? '(1)))
  (ensure (not (bard::length=1? '(1 2)))))

;;; mappend
(addtest test-mappend
  (ensure (null (bard::mappend (constantly nil) nil)))
  (ensure (= 8 (length (bard::mappend #'(lambda (x)(list (oddp x)))
                                      (list 1 2 3 4 5 6 7 8))))))

;;; drop
(addtest test-drop
  (ensure (null (bard::drop 0 nil)))
  (ensure-same (bard::drop 1 (list 0 1 2 3))
               (list 1 2 3))
  (ensure-same (bard::drop 4 (list 0 1 2 3))
               nil)
  (ensure-error (bard::drop 4 (list 0))))

;;; take
(addtest test-take
  (ensure (null (bard::take 0 nil)))
  (ensure-same (bard::take 1 (list 0 1 2 3))
               (list 0))
  (ensure-same (bard::take 4 (list 0 1 2 3))
               (list 0 1 2 3))
  (ensure-error (bard::take 4 nil)))

;;; starts-with?
(addtest test-starts-with?
  (ensure (bard::starts-with? (list 0 1 2 3) 0))
  (ensure (not (bard::starts-with? (list 0 1 2 3) 1001)))
  (ensure (not (bard::starts-with? nil 1001))))

;;; make-true-list
(addtest test-make-true-list 
  (ensure (null (cdr (last (bard::make-true-list nil)))))
  (ensure (null (cdr (last (bard::make-true-list '(1 2 3 . 4))))))
  (ensure (null (cdr (last (bard::make-true-list '(1 2 3 4)))))))

;;; times
(addtest test-times
  (ensure (null (bard::times 0 1)))
  (ensure (= 6 (length (bard::times 6 1)))))

;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'util-tests))
