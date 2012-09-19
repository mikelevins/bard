;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          string-utils.scm
;;;; Project:       Bard
;;;; Purpose:       general-purpose string utilities 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (str . args)
  (if (null? args)
      ""
      (if (null? (cdr args))
          (if (string? (car args))
              (car args)
              (object->string (car args)))
          (string-append (str (car args))
                         (apply str (cdr args))))))
