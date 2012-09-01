;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          variable.scm
;;;; Project:       Bard
;;;; Purpose:       compiling variable references
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%variable-ref? expr) 
  (symbol? expr))

(define (%compile-variable-ref expr env) 
  (receive (i j)(find-in-env expr env)
           (if (and i j)
               (%gen 'LREF i j)
               (%gen 'MREF expr))))

;;; (define $env (env-add-frame (null-env) (make-env-frame (list (make-binding 'x 5)))))
;;; (%compile-variable-ref 'x $env)
;;; (%compile-variable-ref 'bard.lang:foo '())
