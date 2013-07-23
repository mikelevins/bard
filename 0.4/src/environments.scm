;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define (default-environment)
  '())

(define (env-ref env varname)
  (assq env varname))

(define (make-environment vars vals)
  (map cons vars vals))

(define (merge-environments . envs)
  (if (null? envs)
      '()
      (if (null? (cdr envs))
          (car envs)
          (append (car envs)
                  (apply merge-environments (cdr envs))))))
