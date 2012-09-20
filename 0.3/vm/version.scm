;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard VM version string
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define $bard-version (vector 0 3 0))

(define (bard:version) $bard-version)

(define (bard:version-string) 
  (string-append (object->string (vector-ref (bard:version) 0))
                 "."
                 (object->string (vector-ref (bard:version) 1))
                 "."
                 (object->string (vector-ref (bard:version) 2))))

