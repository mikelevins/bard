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
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol StructureValue)

;;; structure-value?
;;; ---------------------------------------------------------------------

(define bard:structure-value? (%make-function name: 'structure-value?))

(%function-add-method! bard:structure-value? `(,Anything) (%method (x) false))
(%function-add-method! bard:structure-value? `(,<input-stream>) (%method (x) true))
(%function-add-method! bard:structure-value? `(,<output-stream>) (%method (x) true))
(%function-add-method! bard:structure-value? `(,<frame>) (%method (x) true))
(%function-add-method! bard:structure-value? `(,<function>) (%method (x) true))
(%function-add-method! bard:structure-value? `(,<method>) (%method (x) true))

