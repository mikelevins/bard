;;;; ***********************************************************************
;;;;
;;;; Name:          types-primitive.scm
;;;; Project:       Bard
;;;; Purpose:       struct <primitive>
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base structs
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <primitive>
;;; ----------------------------------------------------------------------

(define tags:$bard-primitive (%next-bard-type-number))
(define <primitive> (make-base-struct '<primitive> tags:$bard-primitive))

;;; constructor

(define (make-primitive #!key
                        (procedure #f)
                        (required-count 0)
                        (restarg #f)
                        (debug-name 'an-anonymous-primitive))
  (let* ((prim (make-primitive-instance <primitive> debug-name #f required-count restarg))
         (prim-proc (lambda args (apply procedure args))))
    (set-primitive-proc! prim prim-proc)
    prim))

;;; accessors

(define primitive-name primitive-instance-name)
(define primitive-proc primitive-instance-proc)
(define set-primitive-proc! primitive-instance-proc-set!)
(define primitive-restarg primitive-instance-restarg)
(define primitive-required-count primitive-instance-required-count)

