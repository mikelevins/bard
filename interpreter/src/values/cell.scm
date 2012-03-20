;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          cell.scm
;;;; Project:       Bard
;;;; Purpose:       cell implementation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; bard cells
;;;---------------------------------------------------------------------

(define (%make-cell value)
  (spawn 
   (lambda ()
     (let loop ((value value))
       (recv
        ((from tag 'ref)
         (! from (list tag value))
         (loop value))
        (('set! value)
         (loop value)))))))

(define (%cell-get cell)
  (!? cell 'ref))

(define (%cell-put! cell val)
  (! cell (list 'set! val)))

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------

(define (bard:cell val)
  (%make-cell val))

(define bard:cell-get %cell-get)
(define bard:cell-put! %cell-put!)

(define (bard:cell? x)
  (thread? x))

