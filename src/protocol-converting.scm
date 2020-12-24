;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-converting.scm
;;;; Project:       Bard
;;;; Purpose:       producing values of one type based on inputs of another
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (%->list x)
  (cond
   ((null? x) x)
   ((pair? x) x)
   ((string? x) (string->list x))
   ((alist-table-instance? x)(alist-table-instance-slots x))
   (else: (error (str "Can't convert " (%as-string x) " to a List")))))

(define-protocol-function Converting as
  signatures: (list (signature (Type Anything) #f (Anything))))


;;; ---------------------------------------------------------------------
;;; as
;;; ---------------------------------------------------------------------

;;; as List
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton List) Anything) 
  (lambda (type thing)(%->list thing)))

;;; as <string>
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton <string>) Anything) 
  (lambda (type thing)(object->string thing)))

(define-primitive-method as ((%singleton <string>) <symbol>) 
  (lambda (type thing)(symbol->string thing)))

(define-primitive-method as ((%singleton <string>) <keyword>) 
  (lambda (type thing)(keyword->string thing)))

(define-primitive-method as ((%singleton <string>) <null>) 
  (constantly ""))

(define-primitive-method as ((%singleton <string>) <pair>) 
  (lambda (type thing)
    (if (list? thing)
        (if (every? char? thing)
            (list->string thing)
            (error (str "Can't convert " (%as-string thing)
                        " to a string; some elements are not characters")))
        (error (str "Can't convert " (%as-string thing) " to a string")))))

;;; as <symbol>
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton <symbol>) <string>) 
  (lambda (type thing)(string->symbol thing)))

(define-primitive-method as ((%singleton <symbol>) <keyword>) 
  (lambda (type thing)(string->symbol (keyword->string thing))))

;;; as <keyword>
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton <keyword>) <string>) 
  (lambda (type thing)(string->keyword thing)))

(define-primitive-method as ((%singleton <keyword>) <symbol>) 
  (lambda (type thing)(string->keyword (symbol->string thing))))

;;; as List
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton List) <string>) 
  (lambda (type thing)(string->list thing)))

;;; as <url>
;;; ---------------------------------------------------------------------

(define-primitive-method as ((%singleton <url>) <string>) 
  (lambda (type thing)
    (error (str "tell mikel to get off his butt and implement the URL parser"))))



