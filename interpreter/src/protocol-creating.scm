;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-creating.scm
;;;; Project:       Bard
;;;; Purpose:       constructing and initializing values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define bard:make (make-function debug-name: 'make
                                 input-types: `(,Type)
                                 restarg: 'more
                                 output-types: `(,Anything)))

;;; Anything

(define (%bard-make-anything type . args)
  (error (str "Don't know how to make an instance of " (%as-string type))))

(%add-primitive-method! bard:make `(,Anything)
                        %bard-make-anything
                        debug-name: 'make)

;;; <alist-table>

(define (%bard-make-alist-table type . args)
  (let ((slots-alist (plist->alist args)))
    (%make-alist-table slots-alist)))

(%add-primitive-method! bard:make `(,(%singleton <alist-table>))
                        %bard-make-alist-table
                        debug-name: 'make)

;;; <function>

(define (%bard-make-function type . args)
  (let ((name (getf debug-name: args default: #f))
        (input-types (getf input-types: args default: '()))
        (output-types (getf output-types: args default: '()))
        (restarg (getf restarg: args default: #f)))
    (make-function debug-name: name
                   input-types: input-types
                   output-types: output-types
                   restarg: restarg)))

(%add-primitive-method! bard:make `(,(%singleton <function>))
                        %bard-make-function
                        debug-name: 'make)

;;; <generator>

(%add-primitive-method! bard:make `(,(%singleton <generator>))
                        (lambda (type . args)
                          (let* ((variables (getf variables: args default: '()))
                                 (vars (map car variables))
                                 (initvals (map (lambda (v)(%eval v '()))
                                                (map cadr variables)))
                                 (body (getf body: args default: '())))
                            (make-generator vars initvals body '())))
                        debug-name: 'make)

;;; <keyword>

(%add-primitive-method! bard:make `(,(%singleton <keyword>))
                        (lambda (type . args)
                          (let ((name (getf name: args default: (symbol->string (gensym)))))
                            (string->keyword name)))
                        debug-name: 'make)

;;; <pair>

(%add-primitive-method! bard:make `(,(%singleton <pair>))
                        (lambda (type . args)
                          (let ((left (getf left: args default: '()))
                                (right (getf right: args default: '())))
                            (cons left right)))
                        debug-name: 'make)

;;; <protocol>

(%add-primitive-method! bard:make `(,(%singleton <protocol>))
                        (lambda (type . args)
                          (let* ((pname (getf name: args default: #f))
                                 (function-specs (getf functions: args default: '()))
                                 (protocol (%make-protocol pname))
                                 (functions-alist (%build-protocol-functions-alist protocol function-specs '())))
                            (for-each (lambda (fname/fn)(%maybe-add-protocol-function! protocol fname/fn))
                                      functions-alist)
                            protocol))
                        debug-name: 'make)

;;; <record>

(%add-primitive-method! bard:make `(,<record>)
                        (lambda (type . args)(instantiate-record type args))
                        debug-name: 'make)

;;; <tuple>

(%add-primitive-method! bard:make `(,<tuple>)
                        (lambda (type . args)(instantiate-tuple type args))
                        debug-name: 'make)



