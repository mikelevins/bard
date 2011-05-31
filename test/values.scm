;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       test simple self-evaluating expressions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (test-constant str)
  (let* ((in (open-input-string str))
         (expr (bard:%read-syntax in))
         (env (make-standard-environment))
         (code (bard:compile expr env))
         (vm (bard:make-vm code env)))
    (vm:%step vm)
    (bard:print-vm vm)
    (vm:print-values vm)))

#|
;;; ----------------------------------------
;;; singletons
;;; ----------------------------------------

;;; undefined
(test-constant "undefined")

;;; nothing
(test-constant "nothing")

;;; ----------------------------------------
;;; booleans
;;; ----------------------------------------

;;; true
(test-constant "true")

;;; false
(test-constant "false")

;;; ----------------------------------------
;;; numbers
;;; ----------------------------------------

|#