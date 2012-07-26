;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%primitive-show x)
  (cond
   ((%undefined? x) "undefined")
   ((%nothing? x) "nothing")
   ((%boolean? x) (if x "true" "false"))
   ((%symbol? x) (symbol->string x))
   ((%keyword? x) (string-append ":" (keyword->string x)))
   (else (object->string x))))

(define (%primitive-print x #!optional (port #f))
  (let ((port (or port (current-output-port))))
    (display (%primitive-show x) port)))
