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

(define bard:as (make-function debug-name: 'as
                               signatures: (list (signature (Type Anything) #f (Anything)))))

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <symbol>) 
                        (lambda (type thing)(symbol->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <symbol>) <string>)
                        (lambda (type thing)(string->symbol thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <keyword>)
                        (lambda (type thing)(keyword->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <keyword>) <string>)
                        (lambda (type thing)(string->keyword thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <pair>) <string>)
                        (lambda (type thing)(string->list thing))
                        debug-name: 'as)

(define (%->list x)
  (cond
   ((null? x) x)
   ((pair? x) x)
   ((string? x) (string->list x))
   ((alist-table-instance? x)(alist-table-instance-slots x))
   ((generator-instance? x)(reverse (generator-instance-results x)))
   ((record-instance? x)(record-instance-slots x))
   ((tuple-instance? x)(vector->list (tuple-instance-slots x)))
   (else: (error (str "Can't convert " (%as-string x) " to a List")))))

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
                        (lambda (type thing)(list->string thing))
                        debug-name: 'as)


