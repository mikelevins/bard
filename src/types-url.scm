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

