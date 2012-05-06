;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          As.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the As protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; TODO
;;; this scheme, which uses a predefined set of conversion tables, 
;;; should be replaced with proper polymorphism and singleton
;;; dispatch.

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol As)

;;; as
;;; ---------------------------------------------------------------------

(define bard:as (%make-function name: 'as))

(%function-add-method! bard:as `(,Anything ,Anything)
                       (%primitive-method (x y)
                                          (if (equal? x (%object->bard-type y))
                                              y
                                              (error (string-append "don't know how to convert "
                                                                    (%as-string (%object->bard-type y))
                                                                    " to "
                                                                    (%as-string x))))))

(%function-add-method! bard:as `(,(%singleton <cons>) ,<string>)(%primitive-method (x y)(string->list y)))
(%function-add-method! bard:as `(,(%singleton <string>) ,<cons>)
                       (%primitive-method (x y)
                                          (if (every? char? y)
                                              (list->string y)
                                              (error (string-append "non-character element when converting a list to a string: "
                                                                    (%as-string y))))))

