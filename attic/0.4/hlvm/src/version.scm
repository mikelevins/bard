;;;; ***********************************************************************
;;;;
;;;; Name:          version.scm
;;;; Project:       Bard
;;;; Purpose:       the bard implementation version number
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-version '(0 4 0 d1))


;;; ----------------------------------------------------------------------

(define (bard-version-string)
  (let ((s (interpose "."
                      (map object->string
                           (list (bard-major-version)
                                 (bard-minor-version)
                                 (bard-patch-version)))))
        (dev (bard-version-dev-stage)))
    (if dev
        (string-append (apply string-append s)
                       (object->string dev))
        (apply string-append s))))

;;; ----------------------------------------------------------------------

(define (bard-major-version)
  (list-ref $bard-version 0))

(define (bard-minor-version)
  (list-ref $bard-version 1))

(define (bard-patch-version)
  (list-ref $bard-version 2))

(define (bard-version-dev-stage)
  (if (> (length $bard-version) 3)
      (list-ref $bard-version 3)
      #f))
