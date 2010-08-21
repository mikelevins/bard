;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-suite.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       Bard tests
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage bard-test
    (:nicknames :bt)
    (:use #:cl #:ccl)
    (:export #:test-bard)
    (:shadowing-import-from #:|bard-internal| 
                            #:= #:false #:false? #:nothing #:nothing? #:something? #:true #:true?)))

(in-package :bt)

(defmacro test (exp)
  `(let ((result ,exp))
     (progn
       (if result
           (format *standard-output* "~%SUCCEEDED: ")
           (format *standard-output* "~%**FAILED*: "))
       (format *standard-output* "~A" ',exp))))

(defun test-bard ()
  (progn
    (format *standard-output* "~%Bard tests~%")
    ;; nothing
    (test (= (nothing)(nothing)))
    (test (not (= 0 (nothing))))
    (test (not (= (nothing) 0)))
    (test (nothing? (nothing)))
    (test (not (something? (nothing))))
    ;; booleans
    (test (true? (true)))
    (test (not (true? (false))))
    (test (false? (false)))
    (test (not (false? (true))))
    (test (= (true)(true)))
    (test (= (false)(false)))
    (format *standard-output* "~%")
    ))