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
         "/src/version.scm"
         "/lib/uuid.scm"
         "/src/list-utils.scm"
         "/src/string-utils.scm"
         "/src/vector-utils.scm"
         "/src/values.scm"
         "/src/globals.scm"
         "/src/environment.scm"
         ;; "/src/primitives.scm"
         ;; "/src/op.scm"
         ;; "/src/return-record.scm"
         ;; "/src/instruction.scm"
         ;; "/src/code.scm"
         ;; "/src/applicable.scm"
         ;; "/src/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


