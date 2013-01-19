;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-addressing.scm
;;;; Project:       Bard
;;;; Purpose:       locating resources
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

;;; url {scheme: Name domain: Name port: Name path: Name query: Name } -> URL
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url
  signatures: (list (signature ()
                               (%alist-table scheme: "file"
                                             domain: "localhost"
                                             port: #f
                                             path: "/"
                                             query: "")
                               (URL))))

(define-primitive-method url () %make-url)

;;; url-domain URL -> Name
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url-domain
  signatures: (list (signature (URL) #f (Name))))

(define-primitive-method url-domain (<url>) url-domain)

;;; url-path URL -> Name
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url-path
  signatures: (list (signature (URL) #f (Name))))

(define-primitive-method url-path (<url>) url-path)

;;; url-port URL -> Name
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url-port
  signatures: (list (signature (URL) #f (Name))))

(define-primitive-method url-port (<url>) url-port)

;;; url-query URL -> Name
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url-query
  signatures: (list (signature (URL) #f (Name))))

(define-primitive-method url-query (<url>) url-query)

;;; url-scheme URL -> Name
;;; ---------------------------------------------------------------------

(define-protocol-function Addressing url-scheme
  signatures: (list (signature (URL) #f (Name))))

(define-primitive-method url-scheme (<url>) url-scheme)
