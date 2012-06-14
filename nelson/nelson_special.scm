;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          nelson_special.scm
;;;; Project:       Bard/Nelson
;;;; Purpose:       bard special forms for Nelson support
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; action
;;; ----------------------------------------------------------------------

(%defspecial 'action
             (lambda (expr env)
               (let* ((body (cdr expr))
                      (method-form `(method (target parameters puzzle) ,@body)))
                 (%eval method-form env))))


