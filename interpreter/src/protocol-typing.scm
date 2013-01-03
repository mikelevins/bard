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

(define bard:boolean?
  (make-primitive 
   procedure: boolean?
   debug-name: 'boolean?
   required-count: 1
   restarg: #f))

(define (%false? x)
  (or (eqv? x #f)
      (null? x)))

(define bard:false?
  (make-primitive 
   procedure: %false?
   debug-name: 'false?
   required-count: 1
   restarg: #f))

(define (%true? x)
  (and (not (%false? x))
       (not (eqv? x #!unbound))))

(define bard:true?
  (make-primitive 
   procedure: %true?
   debug-name: 'true?
   required-count: 1
   restarg: #f))

(define bard:char?
  (make-primitive
   procedure: char?
   debug-name: 'char?
   required-count: 1
   restarg: #f))

(define bard:float?
  (make-primitive
   procedure: flonum?
   debug-name: 'float?
   required-count: 1
   restarg: #f))

(define bard:foreign-value?
  (make-primitive
   procedure: ##foreign?
   debug-name: 'foreign-value?
   required-count: 1
   restarg: #f))

(define bard:function?
  (make-primitive
   procedure: function?
   debug-name: 'function?
   required-count: 1
   restarg: #f))

(define bard:input-stream?
  (make-primitive
   procedure: input-port?
   debug-name: 'input-stream?
   required-count: 1
   restarg: #f))

(define bard:iostream?
  (make-primitive
   procedure: (lambda (x)(or (input-port? x)(output-port? x)))
   debug-name: 'iostream?
   required-count: 1
   restarg: #f))

(define bard:output-stream?
  (make-primitive
   procedure: output-port?
   debug-name: 'output-stream?
   required-count: 1
   restarg: #f))

(define bard:integer?
  (make-primitive
   procedure: integer?
   debug-name: 'integer?
   required-count: 1
   restarg: #f))

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

