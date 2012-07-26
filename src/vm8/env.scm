;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard VM
;;;; Purpose:       environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; Each variable is stored in a lexical frame, which is represented
;;; as a list, so that variables can be added freely.
;;;
;;; a lexical environment is a list of frames. The vm can therefore
;;; extend an enclosing environment nondestructively by consing a
;;; frame onto the enclosing environment. different control paths
;;; or threads of control can therefore extend the same
;;; inherited environment without conflict

(define (null-env) '())

(define (make-env-frame vals)
  (map identity vals))

(define (extend-env env frame)
  (cons frame env))

(define (enclosing-env env)
  (cdr env))

(define (lref env i j)
  (list-ref (list-ref enf i) j))

(define (lset! env i j v)
  (let* ((frame (list-ref env i))
         (cell (drop j frame)))
    (set-car! cell v)))



