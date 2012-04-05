;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          frame.scm
;;;; Project:       Bard
;;;; Purpose:       the <frame> representation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-macros.scm")

;;; ---------------------------------------------------------------------
;;; <frame>
;;; ---------------------------------------------------------------------

(define-type %frame
  id: 08C172EF-8046-4ADC-BC26-86E4244C9F5A
  constructor: %private-make-frame
  (slots %frame-slots %set-frame-slots!))

(define (%make-frame key-val-plist)
  (let loop ((kvs key-val-plist)
             (slots '()))
    (if (null? kvs)
        (%private-make-frame slots)
        (if (null? (cdr kvs))
            (error "malformed inputs to make-frame" key-val-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define (->frame . kvs)(%make-frame kvs))

(%define-structure-type <frame> %frame?)

(define (%frame-add-slot fr key val)
  (%make-frame (cons (cons key val) 
                     (%frame-slots fr))))

(define (%frame-add-slots fr . kv-plist)
  (let loop ((kvs kv-plist)
             (slots (%frame-slots fr)))
    (if (null? kvs)
        (%make-frame slots)
        (if (null? (cdr kvs))
            (error "odd number of arguments" kv-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define $frame-for-nonframe-value-table (make-table test: equal?))

(define (%ensure-frame-for-nonframe-value! tp . kv-plist)
  (let ((fr (table-ref $frame-for-nonframe-value-table tp #f)))
    (or fr
        (let* ((kvs `(type: ,tp ,@kv-plist))
               (fr (%make-frame kvs)))
          (table-set! $frame-for-nonframe-value-table tp fr)
          fr))))
