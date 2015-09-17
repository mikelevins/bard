;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-macros.scm
;;;; Project:       Bard
;;;; Purpose:       
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (define-protocol-function protocol fname #!key (scheme-name #f) (signatures '()))
  (let ((scheme-name (or scheme-name
                         (string->symbol (string-append "bard:" (symbol->string fname))))))
    `(begin
       (define ,scheme-name
         (make-function debug-name: ',fname
                        signatures: ,signatures))
       (%protocol-add! ,protocol ',fname ,scheme-name))))

(define-macro (define-primitive-method fname types method-fn #!key (scheme-name #f))
  (let ((scheme-name (or scheme-name
                         (string->symbol (string-append "bard:" (symbol->string fname))))))
    `(%add-primitive-method! ,scheme-name (list ,@types)
                        ,method-fn
                        debug-name: ',fname)))
