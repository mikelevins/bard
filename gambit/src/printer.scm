;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.scm
;;;; Project:       bard
;;;; Purpose:       the bard printer
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (bard:format-object x)
  (cond
   ((<syntax-atom>:syntax-atom? x) (<syntax-atom>:format x))
   ((<syntax-sequence>:syntax-sequence? x) (<syntax-sequence>:format x))
   ((<eof>:eof? x) "eof")
   ((<undefined>:undefined? x) "undefined")
   ((<nothing>:nothing? x) "nothing")
   (else (string-append "#<unrecognized object: " (object->string x) ">"))))

