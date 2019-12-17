;;;; ***********************************************************************
;;;;
;;;; Name:          types-url.scm
;;;; Project:       Bard
;;;; Purpose:       struct <url>
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; base structs
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <url>
;;; ----------------------------------------------------------------------

(define tags:$bard-url (%next-bard-type-number))
(define <url> (make-base-struct '<url> tags:$bard-url))


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

