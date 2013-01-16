;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-mapping.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values into lists of key/value pairs
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; type discrimination
;;; ---------------------------------------------------------------------

(define (%Table? x)(not (eq? x #!unbound)))

;;; ---------------------------------------------------------------------
;;; the Table abstraction: all values are (virtually) tables
;;; ---------------------------------------------------------------------
;;; All values are tables.
;;;
;;; every Bard value supports the Mapping protocol, and so can be
;;; treated as a Table. Several types are represented as concrete
;;; associations between explicit keys and values, and supporting the
;;; Mapping protocol for these values is simple and straightforward.
;;; Besides schemas like <alist-table> that are specifically designed 
;;; as representations of tables, records can also be straightforwardly
;;; treated as tables.
;;;
;;; The schemas that are represented as ordered sequences are only
;;; slightly more complicated: their implementations of Mapping
;;; functions treat indexes into the sequence as if they were keys in
;;; a table. This treatment also extends to tuples. It also extends
;;; partially to types like generators and input streams; they support
;;; parts of the Listing protocol, and the same technique of treating
;;; indexes as keys works for those types for which indexes can be
;;; computed.
;;;
;;; Mapping support in Bard's remaining types is less straightfoward.
;;; For types that are not obviously tables or lists, Bard adopts a
;;; fiction similar to Smalltalk's fiction that everything is an
;;; object, and supports it in a similar way.
;;;
;;; Take the integer 5, for example. Bard supports applying the
;;; Mapping functions to the number, as if it were represented by a
;;; finite map of some kind. In fact, of course, it's really an
;;; immediate integer with no internal structure, but Bard presents
;;; the programmer with the fiction that it's a Table. You can call
;;; get-key to fetch "components" of 5; you can call keys to list its
;;; keys; and so on.
;;;
;;; Because 5 has no internal structure, (get-key 5 key) returns
;;; nothing for any key.
;;;
;;; Importantly, though, you can also use put-key to add key/value
;;; associations to 5. The result of doing that isn't the number 5;
;;; it's a newly-created instance of a concrete table type, like
;;; <alist-table>. Besides the key/value pair you explicitly add to
;;; this new table, it also contains a reference to the original value
;;; used to create it, stored on the key value:.

(##include "type-signature-macros.scm")

;;; ---------------------------------------------------------------------
;;; get-key
;;; ---------------------------------------------------------------------

(define bard:get-key
  (make-function debug-name: 'get-key
                 signatures: (list (signature (Table Anything) #f (Anything)))))

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(define bard:keys
  (make-function debug-name: 'keys
                 signatures: (list (signature (Table) #f (List)))))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(define bard:merge
  (make-function debug-name: 'merge
                 signatures: (list (signature (Table Table) #f (Table)))))

;;; ---------------------------------------------------------------------
;;; put-key
;;; ---------------------------------------------------------------------

(define bard:put-key
  (make-function debug-name: 'put-key
                 signatures: (list (signature (Table Anything Anything) #f (Anything)))))

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(define bard:vals
  (make-function debug-name: 'vals
                 signatures: (list (signature (Table) #f (List)))))


