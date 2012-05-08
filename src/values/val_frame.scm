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

(define-type %frame
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  constructor: %private-make-frame
  (slots %frame-slots %set-frame-slots!))

(%define-structure-type <frame> %frame?)

(define (%make-frame key-val-plist)
  (%private-make-frame (reverse (plist->alist key-val-plist))))

(define (->frame . kvs)(%make-frame kvs))

(define (%frame-slot? x)
  (and (list? x)
       (not (null? x))
       (not (null? (cdr x)))
       (null? (cddr x))))

(define (%list->frame ls)
  (%private-make-frame (map (lambda (item)(cons (car item)(cadr item)))
                            ls)))

(define (%frame->list fr)
  (map (lambda (slot)(list (car slot)(cdr slot)))
       (%frame-slots fr)))

;;; private slot-management util
;;; returns new slot list in reverse order
(define (%frame-slots-remove-slot slots key)
  (let loop ((old-slots slots)
             (new-slots '()))
    (if (null? old-slots)
        new-slots
        (let ((slot (car old-slots)))
          (if (equal? key (car slot))
              (loop (cdr old-slots)
                    new-slots)
              (loop (cdr old-slots)
                    (cons slot new-slots)))))))

(define (%frame-remove-slot fr key)
  (%private-make-frame (reverse (%frame-slots-remove-slot (%frame-slots fr) key))))

(define (%frame-add-slot-last fr key val)
  (%private-make-frame (reverse (cons (cons key val)
                                      (%frame-slots-remove-slot (%frame-slots fr) key)))))

(define (%frame-add-slot-first fr key val)
  (%private-make-frame (cons (cons key val)
                             (reverse (%frame-slots-remove-slot (%frame-slots fr) key)))))

(define (%frame-find-slot fr key)
  (assoc key (%frame-slots fr)))

(define (%frame-get fr key #!optional (default (%nothing)))
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
  (let loop ((old-slots (%frame-slots fr))
             (new-slots '()))
    (if (null? old-slots)
        (%private-make-frame (reverse (cons (cons key val) new-slots)))
        (let ((old-slot (car old-slots)))
          (if (equal? (car old-slot) key)
              (loop (cdr old-slots) new-slots)
              (loop (cdr old-slots) (cons old-slot new-slots)))))))

(define (%keys frame)
  (map car (%frame-slots frame)))

(define (%vals frame)
  (map cdr (%frame-slots frame)))