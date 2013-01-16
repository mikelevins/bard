;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-typing.scm
;;;; Project:       Bard
;;;; Purpose:       discriminating values by type
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")
(declare (standard-bindings))

;;; list?

(define bard:list? (make-function debug-name: 'list?
                                  signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:list?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <pair>)
                        (lambda (ls)(list? ls))
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <string>)
                        (constantly #t)
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <alist-table>)
                        (constantly #t)
                        debug-name: 'list?)

(define bard:symbol? (make-function debug-name: 'symbol?
                                    signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:symbol?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'symbol?)

(%add-primitive-method! bard:symbol?
                        (list <symbol>)
                        (constantly #t)
                        debug-name: 'symbol?)

(define bard:nothing? (make-function debug-name: 'nothing?
                                     signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:nothing?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'nothing?)

(%add-primitive-method! bard:nothing?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'nothing?)

(define bard:something? (make-function debug-name: 'something?
                                       signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:something?
                        (list Anything)
                        (constantly #t)
                        debug-name: 'something?)

(%add-primitive-method! bard:something?
                        (list <null>)
                        (constantly #f)
                        debug-name: 'something?)

(define bard:table? (make-function debug-name: 'table?
                                   signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:table?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'table?)

(%add-primitive-method! bard:table?
                        (list <alist-table>)
                        (constantly #t)
                        debug-name: 'table?)

(define bard:text? (make-function debug-name: 'text?
                                  signatures: (list (signature (Anything) #f (Boolean)))))

(%add-primitive-method! bard:text?
                        (list Anything) 
                        (constantly #f)
                        debug-name: 'text?)

(%add-primitive-method! bard:text?
                        (list <string>) 
                        (constantly #t)
                        debug-name: 'text?)

(%add-primitive-method! bard:text?
                        (list <symbol>) 
                        (constantly #t)
                        debug-name: 'text?)

(%add-primitive-method! bard:text?
                        (list <keyword>) 
                        (constantly #t)
                        debug-name: 'text?)

(define bard:type (make-function debug-name: 'type
                                 signatures: (list (signature (Anything) #f (Type)))))

(%add-primitive-method! bard:type
                        (list Anything) 
                        (lambda (x)(%value->schema x))
                        debug-name: 'type)
