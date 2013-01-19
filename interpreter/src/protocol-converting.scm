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

(##include "type-signature-macros.scm")

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (%->list x)
  (cond
   ((null? x) x)
   ((pair? x) x)
   ((string? x) (string->list x))
   ((alist-table-instance? x)(alist-table-instance-slots x))
   ((record-instance? x)(record-instance-slots x))
   ((tuple-instance? x)(vector->list (tuple-instance-slots x)))
   (else: (error (str "Can't convert " (%as-string x) " to a List")))))

(define bard:as (make-function debug-name: 'as
                               signatures: (list (signature (Type Anything) #f (Anything)))))

;;; ---------------------------------------------------------------------
;;; as
;;; ---------------------------------------------------------------------

;;; as <string>
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <symbol>) 
                        (lambda (type thing)(symbol->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <keyword>)
                        (lambda (type thing)(keyword->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton List) Anything)
                        (lambda (type thing)(%->list thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <null>)
                        (constantly "")
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <pair>)
                        (lambda (type thing)
                          (if (list? thing)
                              (if (every? char? thing)
                                  (list->string thing)
                                  (error (str "Can't convert " (%as-string thing)
                                              " to a string; some elements are not characters")))
                              (error (str "Can't convert " (%as-string thing) " to a string"))))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <symbol>)
                        (lambda (type thing)(symbol->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <keyword>)
                        (lambda (type thing)(keyword->string thing))
                        debug-name: 'as)

;;; as <symbol>
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:as
                        (list (%singleton <symbol>) <string>)
                        (lambda (type thing)(string->symbol thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <symbol>) <keyword>)
                        (lambda (type thing)(string->symbol (keyword->string thing)))
                        debug-name: 'as)

;;; as <keyword>
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:as
                        (list (%singleton <keyword>) <string>)
                        (lambda (type thing)(string->keyword thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <keyword>) <symbol>)
                        (lambda (type thing)(string->keyword (symbol->string thing)))
                        debug-name: 'as)

;;; as List
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:as
                        (list (%singleton List) <string>)
                        (lambda (type thing)(string->list thing))
                        debug-name: 'as)

;;; as <url>
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:as
                        (list (%singleton <url>) <string>)
                        (lambda (type thing)
                          (error (str "tell mikel to get off his butt and implement the URL parser")))
                        debug-name: 'as)


