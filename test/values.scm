;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       test simple self-evaluating expressions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------
;;; singletons
;;; ----------------------------------------

;;; undefined

(define $undefined-string "undefined")
(define $in (open-input-string $undefined-string))
(define $expr (bard:%read-syntax $in))
(close-input-port $in)
(define $env (make-standard-environment))
(define $code (bard:compile $expr $env))
(define $vm (bard:make-vm $code $env))
(vm:%next $vm)
(vm:print-values $vm)

;;; nothing

(define $nothing-string "nothing")
(define $in (open-input-string $nothing-string))
(define $expr (bard:%read-syntax $in))
(close-input-port $in)
(define $env (make-standard-environment))
(define $code (bard:compile $expr $env))
(define $vm (bard:make-vm $code $env))
(vm:%next $vm)
(vm:print-values $vm)

;;; ----------------------------------------
;;; booleans
;;; ----------------------------------------

;;; true

(define $true-string "true")
(define $in (open-input-string $true-string))
(define $expr (bard:%read-syntax $in))
(close-input-port $in)
(define $env (make-standard-environment))
(define $code (bard:compile $expr $env))
(define $vm (bard:make-vm $code $env))
(vm:%next $vm)
(vm:print-values $vm)

;;; false

(define $false-string "false")
(define $in (open-input-string $false-string))
(define $expr (bard:%read-syntax $in))
(close-input-port $in)
(define $env (make-standard-environment))
(define $code (bard:compile $expr $env))
(define $vm (bard:make-vm $code $env))
(vm:%next $vm)
(vm:print-values $vm)

;;; ----------------------------------------
;;; numbers
;;; ----------------------------------------
