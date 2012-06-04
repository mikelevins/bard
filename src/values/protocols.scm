;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocols.scm
;;;; Project:       Bard
;;;; Purpose:       built-in protocols
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


;;; ---------------------------------------------------------------------
;;; Common utilities
;;; ---------------------------------------------------------------------

(define %yes (constantly (%true)))
(define %no (constantly (%false)))

;;; ---------------------------------------------------------------------
;;; Applicable
;;; ---------------------------------------------------------------------

(define (%applicable? thing)
  (or 
   (%keyed-collection? thing)
   (%primitive-method? thing)
   (%interpreted-method? thing)
   (%function? thing)
   (procedure? thing)))

(define bard:applicable?
  (%make-primitive-method %applicable?
   name: 'applicable?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:apply
  (%make-primitive-method %apply
   name: 'apply
   parameters: (%list 'op 'args)
   required-count: 2
   restarg: #f))


;;; ---------------------------------------------------------------------
;;; As
;;; ---------------------------------------------------------------------

(define bard:as (%make-function name: 'as))

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <symbol>)
                        (%list 'type 'thing)
                        (lambda (type thing)(symbol->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <symbol>) <string>)
                        (%list 'type 'thing)
                        (lambda (type thing)(string->symbol thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <keyword>)
                        (%list 'type 'thing)
                        (lambda (type thing)(keyword->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <keyword>) <string>)
                        (%list 'type 'thing)
                        (lambda (type thing)(string->keyword thing))
                        name: 'as)

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(define bard:boolean?
  (%make-primitive-method %boolean?
   name: 'boolean?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:false?
  (%make-primitive-method %false?
   name: 'false?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:true?
  (%make-primitive-method %true?
   name: 'true?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

(define bard:character?
  (%make-primitive-method %character?
   name: 'character?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Equal
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))

(%add-primitive-method! bard:=
                        (%list Anything Anything)
                        (%list 'apple 'orange)
                        equal?
                        name: '=)

;;; ---------------------------------------------------------------------
;;; Float
;;; ---------------------------------------------------------------------

(define bard:float?
  (%make-primitive-method flonum?
   name: 'float?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; ForeignValue?
;;; ---------------------------------------------------------------------

(define bard:foreign-value?
  (%make-primitive-method ##foreign?
   name: 'foreign-value?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Frame
;;; ---------------------------------------------------------------------

;;; frame?

(define bard:frame?
  (%make-primitive-method %frame?
   name: 'frame?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; get

(define bard:get (%make-function name: 'get))

(%add-primitive-method! bard:get
                        (%list <frame> Anything)
                        (%list 'frame 'key)
                        (lambda (frame key)(%frame-get frame key (%nothing)))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <null> <fixnum>)
                        (%list 'ls 'n)
                        (lambda (ls n)(error (string-append "Index out of range: " (object->string n))))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <list> <fixnum>)
                        (%list 'ls 'n)
                        (lambda (ls n)(%list-ref ls n))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <string> <fixnum>)
                        (%list 'str 'n)
                        (lambda (str n)(string-ref str n))
                        name: 'get)

;;; keys

(define bard:keys (%make-function name: 'keys))

(%add-primitive-method! bard:keys
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)(%frame-keys frame))
                        name: 'keys)
;;; put

(define bard:put (%make-function name: 'put))

(%add-primitive-method! bard:put
                        (%list <frame> Anything Anything)
                        (%list 'frame 'key 'val)
                        %frame-put
                        name: 'put)

;;; ---------------------------------------------------------------------
;;; IOStream
;;; ---------------------------------------------------------------------

(define bard:current-input
  (%make-primitive-method current-input-port
   name: 'current-input
   parameters: %nil
   required-count: 0
   restarg: #f))

(define bard:current-output
  (%make-primitive-method current-output-port
   name: 'current-output
   parameters: %nil
   required-count: 0
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

(define bard:length (%make-function name: 'length))

(%add-primitive-method! bard:length
                        (%list <null>)
                        (%list 'ls)
                        (constantly 0)
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <list>)
                        (%list 'ls)
                        %length
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <string>)
                        (%list 'string)
                        string-length
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)(%length (%frame-keys frame)))
                        name: 'length)

(define bard:last (%make-function name: 'last))

(%add-primitive-method! bard:last
                        (%list <list>)
                        (%list 'ls)
                        %last
                        name: 'last)

(%add-primitive-method! bard:last
                        (%list <string>)
                        (%list 'string)
                        (lambda (string)(string-ref string (- (string-length string) 1)))
                        name: 'last)

(%add-primitive-method! bard:last
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)
                          (let* ((keys (%frame-keys frame))
                                 (key (%list-ref keys (- (%length keys) 1))))
                            (%frame key (%frame-get frame key (%nothing)))))
                        name: 'last)


(define bard:append (%make-function name: 'append))

(%add-primitive-method! bard:append
                        (%list <null>  <null>)
                        (%list 'ls1 'ls2)
                        (constantly (%nothing))
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <null>  <list>)
                        (%list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls2)
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <list>  <null>)
                        (%list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls1)
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <list>  <list>)
                        (%list 'ls1 'ls2)
                        %append
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <string>  <string>)
                        (%list 'str1 'str2)
                        string-append
                        name: 'append)


;;; ---------------------------------------------------------------------
;;; Ordered
;;; ---------------------------------------------------------------------

  ;;(%defglobal '> bard:>)
  ;;(%defglobal '< bard:<)
  ;;(%defglobal '>= bard:>=)
  ;;(%defglobal '<= bard:<=)
