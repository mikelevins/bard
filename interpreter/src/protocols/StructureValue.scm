;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          StructureValue.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the StructureValue protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol StructureValue)

;;; structure-value?
;;; ---------------------------------------------------------------------

(define bard:structure-value? (%make-function name: 'structure-value?))

(%function-add-method! bard:structure-value? `(,Anything) (lambda (x)(bard:false)))
(%function-add-method! bard:structure-value? `(,<input-stream>) (lambda (x)(bard:true)))
(%function-add-method! bard:structure-value? `(,<output-stream>) (lambda (x)(bard:true)))
(%function-add-method! bard:structure-value? `(,<frame>) (lambda (x)(bard:true)))
(%function-add-method! bard:structure-value? `(,<function>) (lambda (x)(bard:true)))
(%function-add-method! bard:structure-value? `(,<method>) (lambda (x)(bard:true)))

