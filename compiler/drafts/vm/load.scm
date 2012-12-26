;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       bard 0.3 system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root  "/Users/mikel/Projects/bard/0.3/")


;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "common/src/utils.scm"
         "common/src/env.scm"
         "common/src/globals.scm"
         "common/src/callable.scm"
         "vm/src/code.scm"
         "vm/src/instr.scm"
         "vm/src/instr-utils.scm"
         "vm/src/vm-aux.scm"
         "vm/src/vm.scm"
         "vm/src/vmshell.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (loadvm)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (loadvm)

