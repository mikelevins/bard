;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test-utils.lisp
;;;; Project:       bard
;;;; Purpose:       utilities for running tests
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ----------------------------------------------------------------------
;;; test data structures
;;; ----------------------------------------------------------------------

(defclass test-suite () (name tests))
(defclass unit-test () (name body succeeded? failure-reason))

(defparameter $test-errors '())
(defun clear-test-errors () 
  (setf $test-errors '()))

(defun add-test-error (test-name err)
  (setf $test-errors (append $test-errors (list (cons test-name err)))))

(defparameter $test-failures '())

(defun clear-test-failures ()
  (set! $test-failures '()))

(defun add-test-failure (test-name msg)
  (setf $test-failures (append $test-failures (list (cons test-name msg)))))

(defparameter $test-successes '())

(defun clear-test-successes ()
  (setf $test-successes '()))

(defun add-test-success (test-name)
  (setf $test-successes (append $test-successes (list test-name))))

(defun init-tests ()
  (clear-test-errors)
  (clear-test-failures)
  (clear-test-successes))

;;; ----------------------------------------------------------------------
;;; running tests
;;; ----------------------------------------------------------------------


(defun run-unit-test (test)
  (flet ((succeed (val)
           (setf (failure-reason test) nil)
           (setf (succeeded? test) t)
           val)
         (fail (msg) 
           (setf (failure-reason test) msg)
           (setf (succeeded? test) nil)
           nil))
    )

(define (run-test-suite s)
  (init-tests)
  (display (format "~%Running test suite '~a' [~a tests]..." (test-suite-name s)(length (test-suite-tests s))))
  (let loop ((tests (test-suite-tests s)))
    (if (null? tests)
        (begin
          (display (format "done." (test-suite-name s)))
          (display (format "~%  ~a tests succeeded" (length $test-successes)))
          (display (format "~%  ~a failures reported" (length $test-failures)))
          (let loop ((fails $test-failures))
            (if (null? fails)
                #f
                (let ((f (car fails)))
                  (display (format "~%  Test: '~a' (~a)" (car f)(cdr f)))
                  (loop (cdr fails)))))
          (display (format "~%  ~a errors reported" (length $test-errors)))
          (let loop ((errs $test-errors))
            (if (null? errs)
                #f
                (let ((e (car errs)))
                  (display (format "~%  Test: '~a' (~a)" (car e)(cdr e)))
                  (loop (cdr errs)))))
          (newline))
        (begin
          (run-unit-test (car tests))
          (loop (cdr tests))))))

(define (unit-test name body) (make-unit-test name body #t #f))
(define (test-suite name . tests) (make-test-suite name tests))