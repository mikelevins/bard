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

(define (str . args)
  (if (null? args)
      ""
      (let ((s (if (string? (car args))
                   (car args)
                   (object->string (car args)))))
        (if (null? (cdr args))
            s
            (string-append s (apply str (cdr args)))))))

