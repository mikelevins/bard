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


(define-type %schema
 constructor: %private-make-schema
 extender: defschema
 (name %schema-name)
 (tag %schema-tag))

;;; ---------------------------------------------------------------------
;;; base schemas
;;; ---------------------------------------------------------------------
;;; primitive types built into the underlying runtime

(defschema %base-schema
 constructor: %make-base-schema)

(define (%define-base-schema name tag)
  (%assert-schema! (%make-base-schema name tag) tag))

;;; ---------------------------------------------------------------------
;;; structures
;;; ---------------------------------------------------------------------
;;; types built into the host runtime, represented by host structures

(define (%define-structure name prototype #!optional (tag (%next-available-schema-tag)))
  (%assert-schema! prototype tag)
  (%assert-structure! name prototype tag)
  prototype)

;;; ---------------------------------------------------------------------
;;; protocols
;;; ---------------------------------------------------------------------

(defschema %protocol
 constructor: %make-protocol
 (functions %protocol-functions %set-protocol-functions!))

(define (%define-protocol name #!optional (tag (%next-available-schema-tag))(functions '()))
  (%assert-schema! (%make-protocol name tag functions) tag))

;;; ---------------------------------------------------------------------
;;; classes
;;; ---------------------------------------------------------------------

(defschema %class constructor: %make-class)

(define (%define-class name #!optional (tag (%next-available-schema-tag)))
  (%assert-schema! (%make-class name tag) tag))

;;; ---------------------------------------------------------------------
;;; singletons
;;; ---------------------------------------------------------------------

(define $bard-singletons (make-table test: equal?))

(defschema %singleton
 constructor: %make-singleton
 (value %singleton-value))

(define (%existing-singleton val)(table-ref $bard-singletons val #f))

(define (%singleton val)
  (let ((found (%existing-singleton val)))
    (or found
        (let ((s (%make-singleton (string-append "singleton " (object->string val))
                                  (%tag val)
                                  val)))
          (table-set! $bard-singletons val s)
          s))))

;;; ---------------------------------------------------------------------
;;; records
;;; ---------------------------------------------------------------------

(define $empty-record-slots '())

(defschema %record
 constructor: %private-make-record
 (slots %record-slots %set-record-slots!)
 (slot-names %record-slot-names %set-record-slot-names!))

(define <record> (%define-structure '<record> (##structure-type (%private-make-record '<record> -1 $empty-record-slots '()))))

(define (%record-all-slots record) (%record-slots record))

(define (%spec->slot spec)
  (let* ((sname (car spec))
         (attrs (cdr spec))
         (default (getf default: attrs (%nothing))))
    (cons sname default)))

(define (%assemble-schema-slots slot-specs)
  (map %spec->slot slot-specs))

(define (%make-schema name slot-specs #!optional (tag (%next-available-schema-tag)))
  (let* ((slots (%assemble-schema-slots slot-specs))
         (slot-names (map car slots))
         (sc (%private-make-schema slots name slot-names tag)))
    (%assert-schema! sc)
    sc))

(defschema %record-instance
 constructor: %private-make-record-instance
 (schema %instance-schema)
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
  (let loop ((specs (%record-slots record))
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

