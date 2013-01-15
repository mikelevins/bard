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

;;; url {scheme: Name domain: Name port: Name path: Name query: Name } -> URL
;;; ---------------------------------------------------------------------

(define bard:url
  (make-function debug-name: 'url
                 signatures: (list (signature ()
                                              (%make-alist-table `((scheme: . "file")
                                                                   (domain: . "localhost")
                                                                   (port: . #f)
                                                                   (path: . "/")
                                                                   (query: . "")))
                                              (URL)))))

(%add-primitive-method! bard:url
                        '()
                        %make-url
                        debug-name: 'url)

;;; url-domain URL -> Name
;;; ---------------------------------------------------------------------

(define bard:url-domain
  (make-function debug-name: 'url-domain
                 signatures: (list (signature (URL) #f (Name)))))

(%add-primitive-method! bard:url-domain
                        (list <url>)
                        url-domain
                        debug-name: 'url-domain)

;;; url-path URL -> Name
;;; ---------------------------------------------------------------------

(define bard:url-path
  (make-function debug-name: 'url-path
                 signatures: (list (signature (URL) #f (Name)))))

(%add-primitive-method! bard:url-path
                        (list <url>)
                        url-path
                        debug-name: 'url-path)

;;; url-port URL -> Name
;;; ---------------------------------------------------------------------

(define bard:url-port
  (make-function debug-name: 'url-port
                 signatures: (list (signature (URL) #f (Name)))))

(%add-primitive-method! bard:url-port
                        (list <url>)
                        url-port
                        debug-name: 'url-port)

;;; url-query URL -> Name
;;; ---------------------------------------------------------------------

(define bard:url-query
  (make-function debug-name: 'url-query
                 signatures: (list (signature (URL) #f (Name)))))

(%add-primitive-method! bard:url-query
                        (list <url>)
                        url-query
                        debug-name: 'url-query)

;;; url-schema URL -> Name
;;; ---------------------------------------------------------------------

(define bard:url-scheme
  (make-function debug-name: 'url-scheme
                 signatures: (list (signature (URL) #f (Name)))))

(%add-primitive-method! bard:url-scheme
                        (list <url>)
                        url-scheme
                        debug-name: 'url-scheme)
