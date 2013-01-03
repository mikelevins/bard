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

;;; list?

(define bard:list? (make-function debug-name: 'list?))

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

(define bard:symbol? (make-function debug-name: 'symbol?))

(%add-primitive-method! bard:symbol?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'symbol?)

(%add-primitive-method! bard:symbol?
                        (list <symbol>)
                        (constantly #t)
                        debug-name: 'symbol?)

(define bard:nothing? (make-function debug-name: 'nothing?))

(%add-primitive-method! bard:nothing?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'nothing?)

(%add-primitive-method! bard:nothing?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'nothing?)

(define bard:something? (make-function debug-name: 'something?))

(%add-primitive-method! bard:something?
                        (list Anything)
                        (constantly #t)
                        debug-name: 'something?)

(%add-primitive-method! bard:something?
                        (list <null>)
                        (constantly #f)
                        debug-name: 'something?)

(define bard:table? (make-function debug-name: 'table?))

(%add-primitive-method! bard:table?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'table?)

(%add-primitive-method! bard:table?
                        (list <alist-table>)
                        (constantly #t)
                        debug-name: 'table?)

(define bard:text? (make-function debug-name: 'text?))

(%add-primitive-method! bard:text?
                        (list Anything) 
                        (constantly #f)
                        debug-name: 'text?)

(%add-primitive-method! bard:text?
                        (list <string>) 
                        (constantly #t)
                        debug-name: 'text?)

