;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       bard
;;;; Purpose:       test the compiler
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (test-comp str)
  (let* ((in (open-input-string str))
         (expr (bard:%read-syntax in)))
    (close-port in)
    (bard:compile expr '() #t #f)))

#|
;;; ----------------------------------------
;;; singletons
;;; ----------------------------------------

;;; undefined
(test-comp "undefined")

;;; nothing
(test-comp "nothing")

;;; ----------------------------------------
;;; booleans
;;; ----------------------------------------

;;; true
(test-comp "true")

;;; false
(test-comp "false")

;;; ----------------------------------------
;;; numbers
;;; ----------------------------------------

(test-comp "5")
(test-comp "1.5")
(test-comp "5/2")

;;; ----------------------------------------
;;; characters
;;; ----------------------------------------

(test-comp "\\c")
(test-comp "\\space")

;;; ----------------------------------------
;;; names
;;; ----------------------------------------

(test-comp "Foo")
(test-comp ":Foo")
(test-comp "bard.core:Foo")

;;; ----------------------------------------
;;; text
;;; ----------------------------------------

(test-comp "\"Foo bar Baz!\"")

;;; ----------------------------------------
;;; sequences
;;; ----------------------------------------

(test-comp "(0 1 2 3)")
(test-comp "[0 1 2 3]")
(test-comp "{0 1 2 3}")

|#