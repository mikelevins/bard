;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          globals.lisp
;;;; Project:       Bard
;;;; Purpose:       tests of global variable implementation
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard-test)

(deftestsuite globals-tests ()())

(addtest test-globals
  (let ((testvar (gensym))
        (testval (gensym)))
    (ensure (bard::undefined? (bard::get-global testvar)))
    (bard::set-global! testvar testval)
    (ensure-same (bard::get-global testvar) 
                 testval)))


;;; (let ((lift:*test-describe-if-not-successful?* t))(run-tests :suite 'globals-tests))
