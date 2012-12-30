;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          schemas.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard schemas
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; base schemas
;;; ---------------------------------------------------------------------

(define-type %schema
 constructor: %private-make-schema
 extender: defschema
 (name %schema-name)
 (tag %schema-tag))

(defschema %base-schema
 constructor: %private-make-base-schema
 (slots %base-schema-slots)
 (slot-names %base-schema-slot-names))

;;; ---------------------------------------------------------------------
;;; records
;;; ---------------------------------------------------------------------

(define $empty-record-slots '())

(defschema %record
 constructor: %private-make-record
 (slots %record-slots %set-record-slots!)
 (slot-names %record-slot-names %set-record-slot-names!))

(define <record> 
  (%define-standard-type '<record> (##structure-type (%private-make-record '<record> -1 $empty-record-slots '()))))

(define (%all-slots schema) (%table-slots schema))

(define (%spec->slot spec)
  (let* ((sname (car spec))
         (attrs (cdr spec))
         (default (getf default: attrs (%nothing))))
    (cons sname default)))

(define (%assemble-schema-slots slot-specs)
  (map %spec->slot slot-specs))

(define (%make-schema name slot-specs)
  (let* ((slots (%assemble-schema-slots slot-specs))
         (slot-names (map car slots))
         (tag (%next-available-type-tag))
         (sc (%private-make-schema slots name slot-names tag)))
    (%assert-type! tag sc)
    sc))

(defschema %record-instance
 constructor: %private-make-record-instance
 (schema %record-schema)
 (slots %instance-slots %set-instance-slots!)
 (slot-names %instance-slot-names %set-instance-slot-names!))

(define (%validate-initargs record initargs)
  (let ((specs (%record-slots record)))
    (let loop ((inits initargs))
      (if (null? inits)
          #t
          (if (null? (cdr inits))
              (error (string-append "Malformed schema init list:"
                                    (object->string initargs)))
              (if (assoc (car inits) specs)
                  (loop (cddr inits))
                  (error (string-append "Unrecognized slot name in initializer:"
                                        (object->string (car inits))))))))))

(define (%make-record record . initargs)
  (%validate-initargs record initargs)
  (let loop ((specs (%record-slots schema))
             (slots '()))
    (if (null? specs)
        (%private-make-record-instance (reverse slots) record)
        (let* ((spec (car specs))
               (sname (car spec))
               (sdefault (cdr spec))
               (init (member sname initargs))
               (sval (if init
                        (cadr init)
                        sdefault)))
          (loop (cdr specs)
                (cons (list sname sval)
                      slots))))))

