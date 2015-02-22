;; Copyright (C) Richard Kelsey, Michael Sperber (2002). All Rights Reserved.
;; Kawa porting copyright (C) Per Bothner (2005).

;; This document and translations of it may be copied and furnished to
;; others, and derivative works that comment on or otherwise explain
;; it or assist in its implementation may be prepared, copied,
;; published and distributed, in whole or in part, without restriction
;; of any kind, provided that the above copyright notice and this
;; paragraph are included on all such copies and derivative
;; works. However, this document itself may not be modified in any
;; way, such as by removing the copyright notice or references to the
;; Scheme Request For Implementation process or editors, except as
;; needed for the purpose of developing SRFIs in which case the
;; procedures for copyrights defined in the SRFI process must be
;; followed, or as required to translate it into languages other than
;; English.

;; The limited permissions granted above are perpetual and will not be
;; revoked by the authors or their successors or assigns.

;; This document and the information contained herein is provided on
;; an "AS IS" basis and THE AUTHOR AND THE SRFI EDITORS DISCLAIM ALL
;; WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY
;; WARRANTY THAT THE USE OF THE INFORMATION HEREIN WILL NOT INFRINGE
;; ANY RIGHTS OR ANY IMPLIED WARRANTIES OF MERCHANTABILITY OR FITNESS
;; FOR A PARTICULAR PURPOSE.

(require 'list-lib)
(module-compile-options warn-undefined-variable: #t
			warn-invoke-unknown-method: #t)
(module-export make-condition-type condition-type? make-condition
	       condition? condition-has-type? condition-ref
               message-condition? serious-condition? error?
               condition-message
               condition-type-field-supertype check-condition-type-field-alist
	       make-compound-condition extract-condition
	       define-condition-type condition
	       &condition &message &serious &error)
(provide 'srfi-35)
(provide 'condition)
(provide 'conditions)

(define-simple-class <condition-type> ()
  (name ::symbol)
  (supertype)
  (fields)
  (all-fields)
  ((*init* name supertype fields all-fields)
   (set! (*:.name (this)) name)
   (set! (*:.supertype (this)) supertype)
   (set! (*:.fields (this)) fields)
   (set! (*:.all-fields (this)) all-fields))
  ((toString) :: <java.lang.String>
   (let ((sbuf (make <java.lang.StringBuffer> "#<condition-type ")))
     (*:append sbuf (symbol->string name))
     (*:append sbuf ">")
     (*:toString sbuf))))

(define (condition-type? obj) :: <boolean>
  (instance? obj <condition-type>))

(define (make-condition-type (name :: <symbol>)
			     (supertype :: <condition-type>)
			     fields)
  :: <condition-type>
  (if (not
       (null? (lset-intersection eq?
                                 (*:.all-fields supertype)
                                 fields)))
      (error "duplicate field name" ))
  (make <condition-type> name supertype fields
	(append (*:.all-fields supertype) fields)))

(define-syntax define-condition-type
  (syntax-rules ()
    ((define-condition-type ?name ?supertype ?predicate
       (?field1 ?accessor1) ...)
     (begin
       (define ?name
         (make-condition-type '?name
                              ?supertype
                              '(?field1 ...)))
       (define (?predicate thing)
         (and (condition? thing)
              (condition-has-type? thing ?name)))
       (define (?accessor1 condition)
         (condition-ref (extract-condition condition ?name)
                        '?field1))
       ...))))

(define (condition-subtype? (subtype  :: <condition-type>)  (supertype :: <condition-type>)) :: <boolean>
  (let recur ((subtype :: <condition-type> subtype))
    (cond ((not subtype) #f)
          ((eq? subtype supertype) #t)
          (else
           (recur (*:.supertype subtype))))))

(define (condition-type-field-supertype (condition-type :: <condition-type>)
					field)
  (let loop ((condition-type :: <condition-type> condition-type))
    (cond ((not condition-type) #f)
          ((memq field (*:.fields condition-type))
           condition-type)
          (else
           (loop (*:.supertype condition-type))))))

; The type-field-alist is of the form
; ((<type> (<field-name> . <value>) ...) ...)
(define-simple-class <condition> (<java.lang.RuntimeException>)
  ((*init* type-field-alist)
   (set! (field (this) 'type-field-alist) type-field-alist))
  (type-field-alist))

(define (condition? obj) :: <boolean>
  (instance? obj <condition>))

(define (make-condition (type :: <condition-type>) . field-plist)
  (let ((alist (let label ((plist field-plist))
                 (if (null? plist)
                            '()
                     (cons (cons (car plist)
                                 (cadr plist))
                           (label (cddr plist)))))))
    (if (not (lset= eq?
                    (*:.all-fields type)
                    (map car alist)))
        (error "condition fields don't match condition type"))
    (make <condition> (list (cons type alist)))))

(define (condition-has-type? condition (type :: <condition-type>)) :: <boolean>
  (let loop ((types (condition-types condition)))
    (or (condition-subtype? (car types) type)
	(loop (cdr types)))))

;; FUTURE: (define throwable-type-field-alist ((&condition)))

(define-syntax condition-type-field-alist
  (syntax-rules ()
    ;; FUTURE:
    ;;((condition-type-field-alist condition)
    ;;  (if (instance? <condition> condition)
    ;;	  (*:.type-field-alist (as <condition> condition))
    ;;	  throwable-type-field-alist))))
    ((condition-type-field-alist condition)
     (*:.type-field-alist (as <condition> condition)))))

(define (condition-ref (condition :: <condition>) field)
  (type-field-alist-ref (condition-type-field-alist condition)
                        field))

(define (type-field-alist-ref type-field-alist field)
  (let loop ((type-field-alist type-field-alist))
    (cond ((null? type-field-alist)
           (error "type-field-alist-ref: field not found"
                  type-field-alist field))
          ((assq field (cdr (car type-field-alist)))
           => cdr)
          (else
           (loop (cdr type-field-alist))))))

(define (make-compound-condition condition-1 . conditions)
  (make <condition>
   (apply append (map condition-type-field-alist
                      (cons condition-1 conditions)))))

(define (extract-condition (condition :: <condition>)
			   (type :: <condition-type>))
  :: <condition>
  (let ((entry (find (lambda (entry)
                              (condition-subtype? (car entry) type))
                            (condition-type-field-alist condition))))
    (if (not entry)
        (error "extract-condition: invalid condition type"
                      condition type))
    (make <condition>
      (list (cons type
                  (map (lambda (field)
                         (assq field (cdr entry)))
                       (*:.all-fields type)))))))

(define-syntax condition
  (syntax-rules ()
    ((condition (?type1 (?field1 ?value1) ...) ...)
     (type-field-alist->condition
      (list
       (cons ?type1
             (list (cons '?field1 ?value1) ...))
       ...)))))

(define (type-field-alist->condition type-field-alist)
  (make <condition>
   (map (lambda (entry)
          (cons (car entry)
                (map (lambda (field)
                       (or (assq field (cdr entry))
                           (cons field
                                 (type-field-alist-ref type-field-alist field))))
                     (*:.all-fields (as <condition-type> (car entry))))))
        type-field-alist)))

(define (condition-types condition)
  (map car (condition-type-field-alist condition)))

(define (check-condition-type-field-alist the-type-field-alist)
  (let loop ((type-field-alist the-type-field-alist))
    (if (not (null? type-field-alist))
        (let* ((entry (car type-field-alist))
               (type :: <condition-type> (car entry))
               (field-alist (cdr entry))
               (fields (map car field-alist))
               (all-fields (*:.all-fields type)))
          (for-each (lambda (missing-field)
                      (let ((supertype
                             (condition-type-field-supertype type missing-field)))
                        (if (not
			     (let loop ((alist the-type-field-alist))
			       (or (condition-subtype? (car (car alist)) supertype)
				   (loop (cdr alist)))))
			    (error "missing field in condition construction"
                                   type
                                   missing-field))))
                    (lset-difference eq? all-fields fields))
          (loop (cdr type-field-alist))))))

(define &condition (make <condition-type> '&condition #f '() '()))

(define-condition-type &message &condition
  message-condition?
  (message condition-message))

(define-condition-type &serious &condition
  serious-condition?)

(define-condition-type &error &serious
  error?)
