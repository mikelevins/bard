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
;;; NOTE: this implementation of frames is known to be inefficient
;;; Known issues:
;;; - slots are represented as alists, which means key lookup is linear
;;; - because key order is required to be stable, adding slots
;;;   requires linear traversals and reversals

(define (%make-frame key-val-plist)
  (let loop ((kvs key-val-plist)
             (slots '())
             (keys '()))
    (if (null? kvs)
        (%private-make-frame (reverse slots))
        (if (null? (cdr kvs))
            (error "malformed inputs to make-frame" key-val-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (if (member k keys)
                  (error "duplicate keys in frame description" key-val-plist)
                  (loop more (cons (cons k v) slots) (cons k keys))))))))

(define (->frame . kvs)(%make-frame kvs))

(define (%frame-slot? x)
  (and (list? x)
       (not (null? x))
       (not (null? (cdr x)))
       (null? (cddr x))))

(define (%list->frame ls)
  (let ((slots (let loop ((items ls)
                          (result '()))
                 (if (null? items)
                     (reverse result)
                     (let* ((item (car items))
                            (more (cdr items)))
                       (if (and (list? item)
                                (not (null? item))
                                (not (null? (cdr item)))
                                (null? (cddr item)))
                           (let ((k (car item))
                                 (v (cadr item)))
                             (loop more (cons (cons k v) result)))
                           (error "invalid slot description" item)))))))
    (%private-make-frame slots)))

(define (%frame->list fr)
  (let* ((keys (map car (%frame-slots fr)))
         (vals (map cdr (%frame-slots fr))))
    (map (lambda (k v) (list k v)) keys vals)))

(%define-structure-type <frame> %frame?)

(define (%frame-add-slot fr key val)
  (let ((keys (map car (%frame-slots fr))))
    (if (member key keys)
        (let* ((matching-key? (lambda (i x)(equal? i (car x))))
               (slots (reverse (cons (cons key val)(reverse (remove key (%frame-slots fr) matching-key?))))))
          (%private-make-frame slots))
        (%private-make-frame (reverse (cons (cons key val) (reverse (%frame-slots fr))))))))

(define (%frame-remove-slot fr key)
  (let* ((matching-key? (lambda (i x)(equal? i (car x))))
         (slots (remove key (%frame-slots fr) matching-key?)))
    (%private-make-frame slots)))

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

(define (%frame-find-slot fr key)
  (assoc key (%frame-slots fr)))

(define (%frame-get fr key #!optional (default (bard:nothing)))
  (let ((slot (%frame-find-slot fr key)))
    (if slot
        (cdr slot)
        default)))

(define (%frame-merge fr1 fr2)
  (let* ((slots1 (%frame-slots fr1))
         (slots2 (%frame-slots fr2))
         (key= (lambda (s1 s2)(equal? (car s1)(car s2))))
         (new-slots1 (filter (lambda (s1)(not (any? (lambda (s2)(key= s1 s2)) slots2)))
                             slots1)))
    (%private-make-frame (append new-slots1 slots2))))

(define (%frame-put fr key val)
  (let ((slot (%frame-find-slot fr key)))
    (if slot
        (%frame-merge fr (%list->frame (list (list key val))))
        (%frame-add-slot fr key val))))

(define bard:frame? %frame?)

(define (%keys frame)
  (map car (%frame-slots frame)))

(define (%vals frame)
  (map cdr (%frame-slots frame)))