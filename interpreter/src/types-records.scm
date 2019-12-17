;;;; ***********************************************************************
;;;;
;;;; Name:          types-records.scm
;;;; Project:       Bard
;;;; Purpose:       representing Bard records
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; record structs
;;; =====================================================================

(define tags:$bard-record (%next-bard-type-number))
(define tags:$bard-record-instance (%next-bard-type-number))

(define (%parse-record-slot-spec slot-spec)
  (let* ((sname (car slot-spec))
         (attrs (cdr slot-spec))
         (default (getf default: attrs default: '()))
         (type (getf type: attrs test: eq? default: Anything)))
    `(,sname default: ,default type: ,type)))

(define (%parse-record-slot-specs slot-specs)
  (map (lambda (spec)
         (cond
          ((symbol? spec) `(,spec default: () type: ,Anything))
          ((pair? spec) (%parse-record-slot-spec spec))
          (else: (error (str "Invalid record-slot specification: " spec)))))
       slot-specs))

(define (make-record name slot-specs)
  (let* ((tag tags:$bard-record)
         (slot-templates (%parse-record-slot-specs slot-specs)))
    (make-record-struct name tag slot-templates)))

;;; instance constructor

(define (initialize-record record-instance . initargs)
  (let* ((inits (plist->alist initargs))
         (initslots (map (lambda (slot)
                           (cons (string->symbol (keyword->string (car slot)))
                                 (cdr slot))) 
                         inits)))
    (let ((slots (record-instance-slots record-instance)))
      (for-each (lambda (init)
                  (let* ((sname (car init))
                         (val (cdr init))
                         (slot (alist-get slots sname test: eq?)))
                    (if slot
                        (set-cdr! slot val)
                        (error (str "No such slot: " sname)))))
                initslots))
    
    record-instance))

;;; TODO: slot templates contain type information.
;;;       currently it's not used, but later
;;;       we can enforce it

(define (%make-record-slot template)
  (let* ((sname (car template))
         (attrs (cdr template))
         (val (getf default: attrs default: '())))
    (cons sname val)))

(define (instantiate-record struct initargs)
  (let* ((slot-templates (record-struct-slots struct))
         (slots (map %make-record-slot slot-templates))
         (instance (make-record-instance struct slots)))
    (apply initialize-record instance initargs)))

;;; instance accessors
;;; TODO: add support for marking slots immutable

(define (record-ref record-instance slot-name #!key (default '()))
  (alist-ref (record-instance-slots record-instance) slot-name default: default))

(define (record-set! record-instance slot-name val)
  (alist-set! (record-instance-slots record-instance) slot-name val test: eq?))

(define (record-put record-instance slot-name val)
  (let* ((slots (record-instance-slots record-instance))
         (snames (map car slots))
         (new-slots (alist-put (record-instance-slots record-instance) slot-name val test: eq?)))
    (if (member slot-name snames)
        ;; we're putting an existing slot, so the result can be of the same record type
        (make-record-instance (instance-struct record-instance) new-slots)
        ;; we're putting a new slot, so the result cannot be the same record tpye; we return a table instead
        (%make-alist-table new-slots))))


