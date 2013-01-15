;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types-classes.scm
;;;; Project:       Bard
;;;; Purpose:       base Bard classes
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base schemas
;;; =====================================================================

;;; ----------------------------------------------------------------------
;;; classes
;;; ----------------------------------------------------------------------

(define tags:$bard-class (%next-bard-type-number))
(define <class> (make-base-schema '<class> tags:$bard-class))


;;; constructor

(define (%make-class name)(make-class-instance <class> name))

;;; definitions of classes
;;; ----------------------------------------------------------------------
;;; convention: class names are nouns

(define & (%make-class '&)) ; the class of optional arguments
(define Anything (%make-class 'Anything))
(define Applicable (%make-class 'Applicable))
(define Boolean (%make-class 'Boolean))
(define Character (%make-class 'Character))
(define Class (%make-class 'Class))
(define Float (%make-class 'Float))
(define Fraction (%make-class 'Fraction))
(define Function (%make-class 'Function))
(define InputStream (%make-class 'InputStream))
(define Integer (%make-class 'Integer))
(define Keyword (%make-class 'Keyword))
(define List (%make-class 'List))
(define Method (%make-class 'Method))
(define Null (%make-class 'Null))
(define Number (%make-class 'Number))
(define Orderable (%make-class 'Orderable))
(define OutputStream (%make-class 'OutputStream))
(define Pair (%make-class 'Pair))
(define Protocol (%make-class 'Protocol))
(define Ratio (%make-class 'Ratio))
(define Schema (%make-class 'Schema))
(define Stream (%make-class 'Stream))
(define Symbol (%make-class 'Symbol))
(define Table (%make-class 'Table))
(define Text (%make-class 'Text))
(define Type (%make-class 'Type))
(define Undefined (%make-class 'Undefined))
(define URL (%make-class 'URL))

