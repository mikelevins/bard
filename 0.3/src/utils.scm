;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       general utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (interpose val items)
  (if (or (null? items)
          (null? (cdr items)))
      items
      (cons (car items)
            (cons val (interpose val (cdr items))))))
