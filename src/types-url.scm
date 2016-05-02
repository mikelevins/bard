;;;; ***********************************************************************
;;;;
;;;; Name:          types-url.scm
;;;; Project:       Bard
;;;; Purpose:       schema <url>
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base schemas
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <url>
;;; ----------------------------------------------------------------------

(define tags:$bard-url (%next-bard-type-number))
(define <url> (make-base-schema '<url> tags:$bard-url))


;;; constructor

(define (%make-url #!key (scheme "file")(username #f)(password #f) (domain "") (port #f) (path "/") (query ""))
  (make-url-instance <url> scheme username password domain port path query))

;;; accessors

(define url-username url-instance-username)
(define url-password url-instance-password)
(define url-domain url-instance-domain)
(define url-path url-instance-path)
(define url-port url-instance-port)
(define url-query url-instance-query)
(define url-scheme url-instance-scheme)

