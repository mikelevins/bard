;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-streaming.scm
;;;; Project:       Bard
;;;; Purpose:       taking values from streams and putting values on streams
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")


(define bard:contents
  (make-function debug-name: 'contents
                 signatures: (list (signature (InputStream) #f (List)))))

;;; drop n stream -- see protocol-listing for function definition

(define bard:lines
  (make-function debug-name: 'lines
                 signatures: (list (signature (InputStream) #f (List)))))

(define bard:put
  (make-function debug-name: 'put
                 signatures: (list (signature (Anything OutputStream) #f (Boolean)))))

(define bard:stream-direction
  (make-function debug-name: 'stream-direction
                 signatures: (list (signature (Stream) #f (IODirection)))))

(define bard:stream-mode
  (make-function debug-name: 'stream-mode
                 signatures: (list (signature (Stream) #f (IOMode)))))

(define bard:stream-type
  (make-function debug-name: 'stream-type
                 signatures: (list (signature (Stream) #f (IOType)))))

;;; take n stream -- see protocol-listing for function definition
;;; take-one stream -- see protocol-listing for function definition
;;; vals stream -- see protocol-mapping for function definition

