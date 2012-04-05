;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type-functions.scm
;;;; Project:       Bard
;;;; Purpose:       functions for working with types
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "~~lib/_gambit#.scm")
(##include "function-macros.scm")

(bard:define-function bard:type (thing))
(bard:define-method bard:type ((thing <undefined>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <null>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <character>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <boolean>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <symbol>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <keyword>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <flonum>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <ratio>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <fixnum>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <bignum>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <closure>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <cons>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <text>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <input-stream>))(%object->bard-type thing))
(bard:define-method bard:type ((thing <output-stream>))(%object->bard-type thing))


