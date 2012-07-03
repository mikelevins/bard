;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          finger-lists.scm
;;;; Project:       Bard
;;;; Purpose:       experimenting with lists built on finger trees
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (fl:empty) '())

(define-type Single read-only: val)

(define-type FingerList
  read-only:
  lcount ldigit mcount mlist rcount rdigit)

(define (fl:empty? x)
  (cond
   ((null? x) #t)
   ((Single? x) #f)
   ((FingerList? x) #f)
   (else (error "Not a FingerList" x))))

(define (fl:lcons3 a b c fls)
  (fl:lcons a (fl:lcons b (fl:lcons c fls))))

(define (fl:lcons x fls)
  (cond
   ((null? fls) (make-Single x))
   ((Single? fls) (make-FingerList 1 (list x) 0 (fl:empty) 1 (list (Single-val fls))))
   ((FingerList? fls) (if (< (FingerList-lcount fls) 4)
                          (make-FingerList (+ 1 (FingerList-lcount fls))
                                           (cons x (FingerList-ldigit fls))
                                           (FingerList-mcount fls)
                                           (FingerList-mlist fls)
                                           (FingerList-rcount fls)
                                           (FingerList-rdigit fls))
                          (make-FingerList 2
                                           (list x (list-ref (FingerList-ldigit fls) 0))
                                           (+ 3 (FingerList-mcount fls))
                                           (fl:lcons3 (list-ref (FingerList-ldigit fls) 1)
                                                      (list-ref (FingerList-ldigit fls) 2)
                                                      (list-ref (FingerList-ldigit fls) 3)
                                                      (FingerList-mlist fls))
                                           (FingerList-rcount fls)
                                           (FingerList-rdigit fls))))
   (else (error "Not a FingerList" fls))))

(define (fl:rcons3 fls a b c)
  (fl:rcons (fl:rcons (fl:rcons fls a) b) c))

(define (fl:rcons fls x)
  (cond
   ((null? fls) (make-Single x))
   ((Single? fls) (make-FingerList 1 (list (Single-val fls)) 0 (fl:empty) 1 (list x)))
   ((FingerList? fls) (if (< (FingerList-rcount fls) 4)
                          (make-FingerList (FingerList-lcount fls)
                                           (FingerList-ldigit fls)
                                           (FingerList-mcount fls)
                                           (FingerList-mlist fls)
                                           (+ 1 (FingerList-rcount fls))
                                           (cons x (FingerList-rdigit fls)))
                          (make-FingerList (FingerList-lcount fls)
                                           (FingerList-ldigit fls)
                                           (+ 3 (FingerList-mcount fls))
                                           (fl:rcons3 (FingerList-mlist fls)
                                                      (list-ref (FingerList-rdigit fls) 3)
                                                      (list-ref (FingerList-rdigit fls) 2)
                                                      (list-ref (FingerList-rdigit fls) 1))
                                           2
                                           (list x (list-ref (FingerList-rdigit fls) 0)))))
   (else (error "Not a FingerList" fls))))


(define (fl:lcar fls)
  (cond
   ((null? fls) (error "empty list"))
   ((Single? fls) (Single-val fls))
   ((FingerList? fls) (car (FingerList-left fls)))
   (else (error "Not a FingerList" fls))))

(define (fl:rcar fls)
  (cond
   ((null? fls) (error "empty list"))
   ((Single? fls) (Single-val fls))
   ((FingerList? fls) (car (FingerList-right fls)))
   (else (error "Not a FingerList" fls))))

(define (fl:length fls)
  (cond
   ((null? fls) 0)
   ((Single? fls) 1)
   ((FingerList? fls) (+ (FingerList-lcount fls)
                         (FingerList-mcount fls)
                         (FingerList-rcount fls)))
   (else (error "Not a FingerList" fls))))
