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
    (bard:print-object expr)
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

|#