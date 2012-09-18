;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          code.scm
;;;; Project:       Bard
;;;; Purpose:       Bard VM code vectors 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (make-code-vector instruction-list)
  (list->vector instruction-list))

(define (code-ref codevec i)
  (vector-ref codevec i))

(define (link-method vmstate method)
  (let* ((params (method-params method))
         (incode (method-code method))
         (outcode (vector-map (lambda (i)(link-instruction vmstate i))
                              incode))
         (env (method-env method)))
    (make-method params incode outcode)))
