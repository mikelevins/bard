;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-alist.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of the alist primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite primitives-alist-tests ()())

(addtest test-alist
  (ensure (bard::alist? (bard::alist 1 2 3 4)))
  (ensure-same (bard::alist.get (bard::alist 1 2 3 4) 1) 2)
  (ensure-same (bard::alist.get (bard::alist.put (bard::alist 1 2 3 4) 1 101) 1) 101)
  (ensure-same (bard::alist.get (bard::alist 1 2 3 4) -1) bard::*undefined*))

(addtest test-alist-keys
  (ensure (every 'oddp (bard::alist.keys (bard::alist 1 2 3 4 5 6 7 8))))
  (ensure (every 'evenp (bard::alist.vals (bard::alist 1 2 3 4 5 6 7 8)))))

(addtest test-alist-merge
  (let* ((m1 (bard::alist 'a 1 'b 2 'c 3))
         (m2 (bard::alist 'c 303 'd 404 'e 505))
         (m3 (bard::alist.merge m1 m2)))
    (ensure (bard::alist? m3))
    (ensure (every 'symbolp (bard::alist.keys m3)))
    (ensure (every 'numberp (bard::alist.vals m3)))
    (ensure-same (bard::alist.get m3 'a) 1)
    (ensure-same (bard::alist.get m3 'c) 303)
    (ensure-same (bard::alist.get m3 'e) 505)
    (ensure-same (bard::alist.get m3 'z) bard::*undefined*)))


;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'primitives-alist-tests))

