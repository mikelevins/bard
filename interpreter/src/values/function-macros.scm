;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function-macros.scm
;;;; Project:       Bard
;;;; Purpose:       conveniences for use with functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-macro (bard:define-function name signature)
  `(define ,name
     (%make-function name: ',name signature: ',signature)))

(define-macro (bard:define-method name signature . body)
  (let ((args (map car signature))
        (sig (list 'quasiquote (map (lambda (p)
                                      (list (car p) (list 'unquote (cadr p))))
                                    signature))))
    `(%add-method! ,name 
                   (%make-method name: ',name
                                 signature: ,sig
                                 method-function: (lambda ,args ,@body)))))

