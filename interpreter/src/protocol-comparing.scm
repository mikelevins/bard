;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-comparing.scm
;;;; Project:       Bard
;;;; Purpose:       comparing for equality
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")

(define bard:= (make-function debug-name: '=
                              signatures: (list (signature () 'more (Boolean)))))

(define (%bard= . args)
  (if (null? args)
      #t
      (if (null? (cdr args))
          #t
          (and (equal? (car args)(cadr args))
               (apply %bard= (cdr args))))))

(%add-primitive-method! bard:=
                        '()
                        %bard=
                        debug-name: '=)



