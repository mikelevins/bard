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

(define bard:as (make-function debug-name: 'as
                               input-classes: `(,Type ,Anything)
                               output-classes: `(,Anything)))

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

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <null>)
                        (constantly "")
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <pair>)
                        (lambda (type thing)(list->string thing))
                        debug-name: 'as)


