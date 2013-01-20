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

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

;;; ---------------------------------------------------------------------
;;; make Type & {} -> Anything
;;; ---------------------------------------------------------------------

(define-protocol-function Creating make
  signatures: (list (signature (Type) (%alist-table) (Anything))))

;;; Anything

(define (%bard-make-anything type . args)
  (error (str "Don't know how to make an instance of " (%as-string type))))

(define-primitive-method make (Anything) 
  %bard-make-anything)

;;; <alist-table>

(define (%bard-make-alist-table type . args)
  (let ((slots-alist (plist->alist args)))
    (%make-alist-table slots-alist)))

(define-primitive-method make ((%singleton <alist-table>)) 
  %bard-make-alist-table)

;;; <function>

(define (%bard-make-function type . args)
  (let ((name (getf debug-name: args default: #f))
        (input-types (getf input-types: args default: '()))
        (output-types (getf output-types: args default: '()))
        (restarg (getf restarg: args default: #f)))
    (make-function debug-name: name
                   signatures: (list (make-signature input-types restarg output-types)))))

(define-primitive-method make ((%singleton <function>)) 
  %bard-make-function)

;;; <generator>

(define-primitive-method make ((%singleton <generator>)) 
  (lambda (type . args)
    (let* ((variables (getf variables: args default: '()))
           (vars (map car variables))
           (initvals (map (lambda (v)(%eval v '()))
                          (map cadr variables)))
           (body (getf body: args default: '())))
      (make-generator vars initvals body '()))))

;;; <keyword>

(define-primitive-method make ((%singleton <keyword>)) 
  (lambda (type . args)
    (let ((name (getf name: args default: (symbol->string (gensym)))))
      (string->keyword name))))

;;; List

(define-primitive-method make ((%singleton <function>)) 
  (lambda (type . args)
    (let ((vals (getf values: args default: '())))
      (copy-tree vals))))

;;; <pair>

(define-primitive-method make ((%singleton <pair>)) 
  (lambda (type . args)
    (let ((left (getf left: args default: '()))
          (right (getf right: args default: '())))
      (cons left right))))

;;; <protocol>

(define-primitive-method make ((%singleton <protocol>)) 
  (lambda (type . args)
    (let* ((pname (getf name: args default: #f))
           (function-specs (getf functions: args default: '()))
           (protocol (%make-protocol pname))
           (functions-alist (%build-protocol-functions-alist protocol function-specs '())))
      (for-each (lambda (fname/fn)(%maybe-add-protocol-function! protocol fname/fn))
                functions-alist)
      protocol)))


;;; <record>

(define-primitive-method make ((%singleton <record>)) 
  (lambda (type . args)(instantiate-record type args)))

;;; <string>

(define-primitive-method make ((%singleton <string>)) 
  (lambda (type . args)
    (let* ((vals (getf values: args default: "")))
      (cond
       ((string? vals) vals)
       ((list? vals)(if (every? char? vals)
                        (list->string vals)
                        (error (str "All <string> values must be characters, but found: " vals))))
       (else: (error (str "Invalid initial values for a <string>: " vals)))))))

;;; <symbol>

(define-primitive-method make ((%singleton <symbol>)) 
  (lambda (type . args)
    (let ((name (getf name: args default: (symbol->string (gensym)))))
      (string->symbol name))))

;;; Table

(define-primitive-method make ((%singleton Table)) 
  %bard-make-alist-table)

;;; <tuple>

(define-primitive-method make (<tuple>) 
  (lambda (type . args)(instantiate-tuple type args)))

;;; <url>

(define-primitive-method make ((%singleton <url>)) 
  (lambda (type . args)(apply %make-url args)))




