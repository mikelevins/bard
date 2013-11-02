;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of the primitives subsystem
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite primitives-tests ()())

(addtest test-primitives
  (let ((primname 'bard-symbols::|cons|)
        (non-primname (gensym)))
    (ensure (bard::primitive? primname (bard::null-environment) 2))
    (ensure (not (bard::primitive? non-primname (bard::null-environment) 2)))))


;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'primitives-tests))
