;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Bard
;;;; Purpose:       representation of vm functions
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; fns
;;; ----------------------------------------------------------------------

(define-structure fn required-parameters rest-parameter body)

(define (make-fn-env fn args)
  (let* ((params (fn-required-parameters fn))
         (restarg (fn-rest-parameter fn))
         (required-count (length params))
         (found-count (length args)))
    (cond
     ((= required-count found-count) (make-environment params args))
     ((< required-count found-count) (if restarg
                                         (let* ((required-args (take required-count args))
                                                (rest (drop required-count args))
                                                (all-params (append params (list restarg)))
                                                (all-args (append required-args (list rest))))
                                           (make-environment all-params all-args))
                                         (error "Too many arguments to function: " fn args)))
     (else (error "Too few arguments to function: " fn args)))))
