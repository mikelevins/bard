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

(define $bard-error #f)

(define-macro (reporting-errors . body)
  (let ((errvar (gensym)))
    `(with-exception-catcher
      ;; error handler
      (lambda (,errvar)(begin (set! $bard-error (error->string ,errvar)) #f))
      ;; body
      (lambda () (begin (set! $bard-error #f) ,@body)))))

(define (api:version)
  (reporting-errors
   (objc:string->NSString $bard-version-string)))

(define (api:init-bard)
  (reporting-errors
   (%init-bard)
   #t))

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
  (reporting-errors
   (let* ((str (objc:NSString->string text)))
     (bard:read-from-string str))))

(define (api:bard-eval expr)
  (reporting-errors
   (%eval expr '())))

(define (api:load-from-string text)
  (reporting-errors
   (let* ((str (objc:NSString->string text)))
     (%bard-load-from-string str))))

(define (api:show expr)
  (reporting-errors
   (objc:string->NSString (%as-string expr))))

(define (api:as-array expr)
  (reporting-errors
   (cond
    ((null? expr) #f)
    ((%list? expr)(objc:list->NSMutableArray expr))
    (else (begin
            (display (string-append "Bard error: cannot convert "
                                    (object->string expr)
                                    " to an NSArray."))
            #f)))))

(define (api:as-integer expr)
  (reporting-errors
   (cond
    ((integer? expr) expr)
    ((flonum? expr) (round (inexact->exact expr)))
    (else (begin
            (display (string-append "Bard error: cannot convert "
                                    (object->string expr)
                                    " to an integer."))
            -1)))))
