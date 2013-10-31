;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-resources.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with files and other resources
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; url-schemes
;;; ---------------------------------------------------------------------

(defparameter *bard-file-scheme* 'bard-symbols::|file|)
(defparameter *bard-http-scheme* 'bard-symbols::|http|)
(defparameter *bard-https-scheme* 'bard-symbols::|https|)

(defun valid-url-scheme? (s)
  (member s '(*bard-file-scheme* *bard-http-scheme* *bard-https-scheme*)))

(deftype url-scheme ()
  `(and symbol
        (satisfies valid-url-scheme?)))

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defmethod url ((spec string))
  (puri:uri spec))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defmethod url.scheme ((url puri:uri))
  (case (puri:uri-scheme url)
    (:FILE *bard-file-scheme*)
    (:HTTP *bard-http-scheme*)
    (:HTTPS *bard-https-scheme*)
    (t (error "Unrecognized URL scheme: ~s" (puri:uri-scheme url)))))

(defmethod url.host ((url puri:uri))
  (puri:uri-host url))

(defmethod url.path ((url puri:uri))
  (puri:uri-path url))

(defmethod url.port ((url puri:uri))
  (puri:uri-port url))

(defmethod url.query ((url puri:uri))
  (puri:uri-query url))

;;; ---------------------------------------------------------------------
;;; converters
;;; ---------------------------------------------------------------------

(defmethod as-url (x) 
  (error "Can't convert ~s to a URL" x))

(defmethod as-url ((x puri:uri)) 
  x)

(defmethod as-url ((x string)) 
  (url x))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------
