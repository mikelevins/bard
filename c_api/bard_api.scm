;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_api.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme API functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file defines Scheme API functions that are called by the C API
;;; functions defined as c-lambdas in bard_c_api.scm

(define (api:version)
  (objc:string->NSString $bard-version-string))

(define (api:init-bard)
  (let ((error-handler (lambda (err)
                         (display (error->string err))
                         #f))
        (initializer (lambda ()
                       (%init-bard)
                       #t)))
    (with-exception-catcher error-handler initializer)))

(define api:$BARD_NULL 0)
(define api:$BARD_BOOLEAN 1)
(define api:$BARD_NUMBER 2)
(define api:$BARD_SYMBOL 3)
(define api:$BARD_KEYWORD 4)
(define api:$BARD_TEXT 5)
(define api:$BARD_LIST 6)
(define api:$BARD_FRAME 7)
(define api:$BARD_UNRECOGNIZED 255)

(define (api:type-for-C obj)
  (cond
   ((null? obj) api:$BARD_NULL)
   ((boolean? obj) api:$BARD_BOOLEAN)
   ((number? obj) api:$BARD_NUMBER)
   ((symbol? obj) api:$BARD_SYMBOL)
   ((keyword? obj) api:$BARD_KEYWORD)
   ((string? obj) api:$BARD_TEXT)
   ((%list? obj) api:$BARD_LIST)
   ((%frame? obj) api:$BARD_FRAME)
   (else api:$BARD_UNRECOGNIZED)))

(define (api:bard-read text)
  (let* ((str (objc:NSString->string text)))
    (bard:read-from-string str)))

(define (api:bard-eval expr)
  (%eval expr '()))
