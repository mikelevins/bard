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
;;; Frame
;;; ---------------------------------------------------------------------

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
;;; put

(define bard:put (%make-function name: 'put))

(%add-primitive-method! bard:put
                        (%list <frame> Anything Anything)
                        (%list 'frame 'key 'val)
                        %frame-put
                        name: 'put)

