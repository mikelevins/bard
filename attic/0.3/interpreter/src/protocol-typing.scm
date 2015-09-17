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

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

;;; class?

(define-protocol-function Typing class?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method class? (Anything)
  (constantly #f))

(define-primitive-method class? (<class>)
  (constantly #t))

;;; keyword?

(define-protocol-function Typing keyword?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method keyword? (Anything)
  (constantly #f))

(define-primitive-method keyword? (<keyword>)
  (constantly #t))

;;; method?

(define-protocol-function Typing method?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method method? (Anything)
  (constantly #f))

(define-primitive-method method? (<interpreted-method>)
  (constantly #t))

;;; pair?

(define-protocol-function Typing pair?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method pair? (Anything)
  (constantly #f))

(define-primitive-method pair? (<pair>)
  (constantly #t))

;;; protocol?

(define-protocol-function Typing protocol?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method protocol? (Anything)
  (constantly #f))

(define-primitive-method protocol? (<protocol>)
  (constantly #t))

;;; undefined?

(define-protocol-function Typing undefined?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method undefined? (Anything)
  (constantly #f))

(define-primitive-method undefined? (<undefined>)
  (constantly #t))

;;; vector?

(define-protocol-function Typing vector?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method vector? (Anything)
  (constantly #f))

(define-primitive-method vector? (<vector>)
  (constantly #t))

;;; list?

(define-protocol-function Typing list?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method list? (Anything)
  (constantly #f))

(define-primitive-method list? (<null>)
  (constantly #t))

(define-primitive-method list? (<pair>)
  (lambda (ls)(list? ls)))

(define-primitive-method list? (<string>)
  (constantly #t))

(define-primitive-method list? (<vector>)
  (constantly #t))

(define-primitive-method list? (<alist-table>)
  (constantly #t))

(define-protocol-function Typing nothing?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method nothing? (Anything)
  (constantly #f))

(define-primitive-method nothing? (<null>)
  (constantly #t))

(define-protocol-function Typing something?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method something? (Anything)
  (constantly #t))

(define-primitive-method something? (<null>)
  (constantly #f))

(define-protocol-function Typing symbol?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method symbol? (Anything)
  (constantly #f))

(define-primitive-method symbol? (<symbol>)
  (constantly #t))

(define-protocol-function Typing table?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method table? (Anything)
  (constantly #f))

(define-primitive-method table? (<alist-table>)
  (constantly #t))

(define-protocol-function Typing text?
  signatures: (list (signature (Anything) #f (Boolean))))

(define-primitive-method text? (Anything) 
  (constantly #f))

(define-primitive-method text? (<string>) 
  (constantly #t))

(define-protocol-function Typing type
  signatures: (list (signature (Anything) #f (Type))))

(define-primitive-method type (Anything) 
  (lambda (x)(%value->schema x)))

