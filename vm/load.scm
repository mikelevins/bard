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

(define $bard-root  "/Users/mikel/Workshop/bard/vm/") ; osx
;;;(define $bard-root  "/home/mikel/Projects/bard/interpreter/") ; Linux

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/version.scm"
         "src/banner.scm"
         "lib/uuid.scm"
         "src/util.scm"
         "src/stack.scm"
         "src/env.scm"
         "src/instructions.scm"
         "src/program.scm"
         "src/bardo.scm"
         "src/bardvm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (loadvm)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (loadvm)
