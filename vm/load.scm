;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard VM
;;;; Purpose:       bardvm system loader
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************


;;; macos
(define $bardvm-root  "/Users/mikel/Workshop/src/bard/vm/")

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (vm-paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bardvm-files
  (vm-paths $bardvm-root 
         "src/version.scm"
         "src/opcodes.scm"
         "src/global.scm"
         "src/env.scm"
         "src/instr.scm"
         "src/fn.scm"
         "src/bardvm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bardvm)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bardvm-files))

;;; (load-bardvm)
