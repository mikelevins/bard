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

(define $bard-root  "/Volumes/ymra/Users/mikel/Projects/bard/bard/")

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
         "lib/srfi101.scm"
         "lib/wttree.scm"
         "src/util/general.scm"
         "src/values/types.scm"
         "src/values/values.scm"
         "src/eval/env.scm"
         "src/values/functions.scm"
         "src/repl/prims.scm"
         "src/read.scm"
         "src/print.scm"
         "src/eval/special.scm"
         "src/eval/macro.scm"
         "src/eval/apply.scm"
         "src/eval/eval.scm"
         "src/repl/error.scm"
         "src/repl/toplevel.scm"
         "src/bard.scm"

         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load (string-append $bard-root "/load.scm"))
;;; (load-bard)
;;; (bard:repl)
