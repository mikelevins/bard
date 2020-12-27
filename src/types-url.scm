;;;; ***********************************************************************
;;;;
;;;; Name:          types-url.scm
;;;; Project:       Bard
;;;; Purpose:       bard-structure <url>
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; <url>
;;; ----------------------------------------------------------------------

(define tags:$bard-url (%next-bard-type-number))
(define <url> (make-base-bard-structure '<url> tags:$bard-url))


;;; constructor

(define (%make-url url-string)
  (make-url-instance <url> url-string))

(define (url s)
  (%make-url s))
