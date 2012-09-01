;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       bard system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root  "/Users/mikel/Projects/bard/0.3")

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/src/util.scm"
         "/src/self-evaluating.scm"
         "/src/env.scm"
         "/src/special.scm"
         "/src/variable.scm"
         "/src/macro.scm"
         "/src/application.scm"
         "/src/gen.scm"
         "/src/compile.scm"
         "/src/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


