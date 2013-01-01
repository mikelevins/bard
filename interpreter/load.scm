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

(define $bard-root  "/Users/mikel/Projects/bard/interpreter/")

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/version.scm"
         "lib/uuid.scm"
         "src/utils.scm"
         "src/singleton-tree.scm"
         "src/tags.scm"
         "src/schema.scm"
         "src/schemas-primitive.scm"
         ;; "src/values.scm"
         ;; "src/env.scm"
         ;; "src/callable.scm"
         ;; "src/prims.scm"
         ;; "src/read.scm"
         ;; "src/print.scm"
         ;; "src/special.scm"
         ;; "src/macro.scm"
         ;; "src/apply.scm"
         ;; "src/compile.scm"
         ;; "src/eval.scm"
         ;; "src/error.scm"
         ;; "src/toplevel.scm"
         ;; "src/protocols.scm"
         ;; "src/bard.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
;;; (%init-bard)
;;; (bard:repl)



