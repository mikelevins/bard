;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values-simple.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of simple value representation
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite values-simple-tests ()())

;;; octet
(addtest test-octet
  (ensure (bard::octet? 0))
  (ensure (bard::octet? 255))
  (ensure (bard::not (bard::octet? 256)))
  (ensure (not (bard::octet? 'foo))))

;;; eof
(addtest test-eof
  (ensure (bard::eof? bard::*eof*))
  (ensure (bard::eof? (bard::eof))))

;;; undefined
(addtest test-undefined
  (ensure (bard::defined? t))
  (ensure (not (bard::defined? bard::*undefined*)))
  (ensure (not (bard::undefined? t)))
  (ensure (bard::undefined? (bard::undefined))))

;;; nothing
(addtest test-nothing
  (ensure (bard::something? t))
  (ensure (bard::nothing? nil))
  (ensure (not (bard::nothing? t))))

;;; booleans
(addtest test-boolean
  (ensure (bard::boolean? (bard::true)))
  (ensure (bard::boolean? (bard::false)))
  (ensure (not (bard::boolean? (bard::nothing))))
  (ensure (bard::true? (bard::true)))
  (ensure (bard::false? (bard::false)))
  (ensure (not (bard::true? (bard::false))))
  (ensure (not (bard::false? (bard::true))))
  (ensure (bard::true? 101))
  (ensure (bard::false? (bard::nothing))))

;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'values-simple-tests))
