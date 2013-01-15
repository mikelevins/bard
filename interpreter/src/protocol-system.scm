;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-system.scm
;;;; Project:       Bard
;;;; Purpose:       system utilities
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(define bard:file
  (make-function debug-name: 'file
                 signatures: (list (signature (URL) #f (Stream)))))

(%add-primitive-method! bard:file
                        (list <string>)
                        bard:string->file-stream
                        debug-name: 'left)

