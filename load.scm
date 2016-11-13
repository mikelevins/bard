;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       bard system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012-2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

;;(define $bard-root  "/home/mikel/Workshop/src/bard") ; linux
(define $bard-root  "/Users/mikel/Workshop/src/bard") ; macos

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/lib/uuid.scm"
         "/src/version.scm"
         "/src/globals.scm"
         "/src/instruction-set.scm"
         "/src/actor.scm"
         "/src/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
;;; (%init-standard-globals)
