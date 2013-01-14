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

(define $bard-root  "/Users/mikel/Projects/bard/interpreter/") ; osx
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
         "lib/uuid.scm"
         "src/utils.scm"
         "src/singleton-tree.scm"
         "src/types.scm"
         "src/env.scm"
         "src/primitives.scm"
         "src/read.scm"
         "src/print.scm"
         "src/special.scm"
         "src/macro.scm"
         "src/apply.scm"
         "src/eval.scm"
         "src/error.scm"
         "src/protocol-comparing.scm"
         "src/protocol-converting.scm"
         "src/protocol-creating.scm"
         "src/protocol-listing.scm"
         "src/protocol-ordering.scm"
         "src/protocol-pairing.scm"
         "src/protocol-typing.scm"
         "src/init.scm"
         "src/bard.scm"
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



