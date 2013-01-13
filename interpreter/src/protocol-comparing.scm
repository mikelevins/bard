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

(define bard:= (make-function debug-name: '=
                              input-types: `()
                              restarg: 'more
                              output-types: `(,Boolean)))

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



