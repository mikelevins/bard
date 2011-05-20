;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cells.scm
;;;; Project:       bard
;;;; Purpose:       mutable cells
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(##include "~~/lib/gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

(define (bard:make-cell value)
  (spawn 
   (lambda ()
     (let loop ((value value))
       (recv
        ((from tag 'ref)
         (! from (list tag value))
         (loop value))
        (('set! value)
         (loop value)))))))

(define (bard:get-cell cell)
  (!? cell 'ref))

(define (bard:put-cell! cell val)
  (! cell (list 'set! val)))

;;; (define $c (bard:make-cell #f))
;;; (bard:get-cell $c)
;;; (bard:put-cell! $c 1001)
;;; (bard:get-cell $c)