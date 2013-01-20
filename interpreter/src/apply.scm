;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apply.scm
;;;; Project:       Bard
;;;; Purpose:       the apply function
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (%apply op args)
  (cond
   ((function? op)(apply (function-proc op) args))
   ((interpreted-method-instance? op)(apply (interpreted-method-proc op) args))
   ((primitive-instance? op)(apply (primitive-proc op) args))
   ((procedure? op)(apply op args))
   (else (error (str "not an applicable object: " op "; args: " args)))))

(define %funcall (lambda (fn . args)(%apply fn args)))
