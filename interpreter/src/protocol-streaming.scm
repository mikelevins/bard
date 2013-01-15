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

(define bard:file (make-function debug-name: 'file
                                 signatures: (list (signature (URL) #f (Stream)))))

(%add-primitive-method! bard:file
                        (list <string>)
                        bard:string->file-stream
                        debug-name: 'left)
