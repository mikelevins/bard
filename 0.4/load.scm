;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       the system loader
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root  "/Users/mikel/Workshop/programming/bard/0.4") ; osx

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/src/utils.scm"
         "/src/version.scm"
         "/src/error.scm"
         "/src/globals.scm"
         "/src/env.scm"
         "/src/values.scm"
         "/src/lambda.scm"
         "/src/reader.scm"
         "/src/printer.scm"
         "/src/kernel.scm"
         "/src/shell.scm"
         "/src/compiler-let.scm"
         "/src/compiler-set!.scm"
         "/src/compiler.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
;;; (globals:init)



