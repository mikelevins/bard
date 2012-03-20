;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          list.scm
;;;; Project:       Bard
;;;; Purpose:       lists (values that participate in the list protocol)
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; private list operations
;;;---------------------------------------------------------------------

(define (%empty-list) ra:null)
(define %empty-list? ra:null?)
(define %list? ra:list?)
(define %->list ra:random-access-list->linear-access-list)

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define bard:list? %list?)

(define (bard:make-list elements)
  (if (null? elements)
      (%empty-list)
      (apply ra:list elements)))

(define (bard:list . elements)
  (bard:make-list elements))

(define (bard:add-first x s)(ra:cons x s))

(define (bard:add-last s x)
  (bard:append s (bard:list x)))

(define (bard:first s)(ra:car s))

(define (bard:rest s)(ra:cdr s))

(define (bard:second s)(ra:caar s))

(define (bard:third s)(ra:caaar s))

(define (bard:fourth s)(ra:caaaar s))

(define (bard:length s)(ra:length s))

(define bard:fold-left fold-left)

(define bard:fold-right fold-right)

(define bard:append ra:append)

(define bard:reverse ra:reverse)

(define (bard:drop n s)(ra:list-tail s n))

(define (bard:take n s) 
  (let loop ((i 0)
             (result (%empty-list)))
    (if (>= i n)
        (bard:reverse result)
        (loop (+ i 1)
              (bard:add-first (bard:element s i) result)))))

(define bard:element ra:list-ref)

(define bard:image ra:map)

;;; (%->list (bard:list 0 1 2))
;;; (%->list (bard:add-first 'foo (bard:list 0 1 2)))
;;; (%->list (bard:add-last (bard:list 0 1 2) 'foo))
;;; (bard:first (bard:list 0 1 2))
;;; (%->list (bard:rest (bard:list 0 1 2)))
;;; (%->list (bard:drop 2 (bard:list 0 1 2 3 4)))
;;; (%->list (bard:take 3 (bard:list 0 1 2 3 4)))
