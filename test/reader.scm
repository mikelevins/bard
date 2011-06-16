;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.scm
;;;; Project:       bard
;;;; Purpose:       test the reader
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (test-read str)
  (let* ((in (open-input-string str))
         (expr (bard:%read-syntax in)))
    (close-port in)
    (display " type: ")(display (debug-name (frame:get expr type:)))
    (newline)
    (display "value: ")(display (object->string (frame:get expr value:)))
    (newline)))

#|
;;; ----------------------------------------
;;; singletons
;;; ----------------------------------------

;;; undefined
(test-read "undefined")

;;; nothing
(test-read "nothing")

;;; ----------------------------------------
;;; booleans
;;; ----------------------------------------

;;; true
(test-read "true")

;;; false
(test-read "false")

;;; ----------------------------------------
;;; numbers
;;; ----------------------------------------

(test-read "5")
(test-read "1.5")
(test-read "5/2")

;;; ----------------------------------------
;;; characters
;;; ----------------------------------------

(test-read "\\c")
(test-read "\\space")

;;; ----------------------------------------
;;; names
;;; ----------------------------------------

(test-read "Foo")
(test-read ":Foo")
(test-read "bard.core:Foo")

;;; ----------------------------------------
;;; text
;;; ----------------------------------------

(test-read "\"Foo bar Baz!\"")

;;; ----------------------------------------
;;; sequences
;;; ----------------------------------------

;;; ----------------------------------------
;;; maps
;;; ----------------------------------------

|#