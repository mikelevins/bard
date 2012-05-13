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

(%define-structure-type <frame> (##structure-type (%private-make-frame '())) %frame?)

(define (%make-frame-slot key val)(list key val))
(define (%frame-slot-key slot)(car slot))
(define (%frame-slot-val slot)(cadr slot))

(define (%key= slot1 slot2)
  (equal? (%frame-slot-key slot1)(%frame-slot-key slot2)))

(define (%slot-list->slots slist)
  (let loop ((maybe-slots slist)
             (slots '()))
    (if (null? maybe-slots)
        slots
        (let ((slot (car maybe-slots)))
          (if (any? (lambda (s)(equal? (%frame-slot-key slot)(%frame-slot-key s))) slots)
              (loop (cdr maybe-slots) slots)
              (loop (cdr maybe-slots) (cons slot slots)))))))

(define (%plist->slots plist)
  (let loop ((plist plist)
             (maybe-slots '()))
    (if (null? plist)
        (%slot-list->slots maybe-slots)
        (if (null? (cdr plist))
            (error (string-append "malformed plist: " (object->string plist)))
            (loop (cddr plist)
                  (cons (list (car plist)(cadr plist)) 
                        maybe-slots))))))

(define (%merge-slots slots1 slots2)
  (append (filter (lambda (s1)(not (any? (lambda (s2)(%key= s1 s2)) slots2)))
                  slots1)
          slots2))

(define (%make-frame key-val-plist)
  (%private-make-frame (%plist->slots key-val-plist)))

(define (->frame . kvs)(%make-frame kvs))

(define (%frame-slot? x)
  (and (list? x)
       (not (null? x))
       (not (null? (cdr x)))
       (null? (cddr x))))

(define (%list->frame ls)
  (%private-make-frame (%slot-list->slots (reverse ls))))

(define (%frame->list fr)(%frame-slots fr))

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
  (%private-make-frame (reverse (cons (list key val)
                                      (%frame-slots-remove-slot (%frame-slots fr) key)))))

(define (%frame-add-slot-first fr key val)
  (%private-make-frame (cons (list key val)
                             (reverse (%frame-slots-remove-slot (%frame-slots fr) key)))))

(define (%frame-find-slot fr key)
  (assoc key (%frame-slots fr)))

(define (%frame-get fr key #!optional (default (%nothing)))
  (let ((slot (%frame-find-slot fr key)))
    (if slot
        (%frame-slot-val slot)
        default)))

(define (%frame-merge fr1 fr2)
  (let* ((slots1 (%frame-slots fr1))
         (slots2 (%frame-slots fr2))
         (key= (lambda (s1 s2)(equal? (car s1)(car s2))))
         (new-slots1 (filter (lambda (s1)(not (any? (lambda (s2)(key= s1 s2)) slots2)))
                             slots1)))
    (%private-make-frame (append new-slots1 slots2))))

(define (%frame-put fr key val)
  (%private-make-frame
   (reverse
    (cons (list key val)
          (%frame-slots-remove-slot (%frame-slots fr) key)))))

(define (%keys frame)
  (map %frame-slot-key (%frame-slots frame)))

(define (%vals frame)
  (map %frame-slot-val (%frame-slots frame)))

;;; utils for treating sequences as frames

(define (%cons-as-frame ls)
  (%private-make-frame (map list (range 0 (length ls)) ls)))

(define (%string-as-frame str)
  (%private-make-frame 
   (let ((ls (string->list str)))
     (map list (range 0 (length ls)) ls))))

