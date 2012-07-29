;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard VM
;;;; Purpose:       vm system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; project root
;;; ----------------------------------------------------------------------
;;; modify if the bard sources are at another pathname

(define $bard-root  "/home/mikel/Projects/bard")

;;; ----------------------------------------------------------------------
;;; set the proper readtable for Bard
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/src/vm/src/types.scm"
         "/src/vm/src/values.scm"
         "/src/vm/src/reader.scm"
         "/src/vm/src/printer.scm"
         "/src/vm/src/env.scm"
         "/src/vm/src/module.scm"
         "/src/vm/src/function.scm"
         "/src/vm/src/macro.scm"
         "/src/vm/src/op.scm"
         "/src/vm/src/compiler.scm"
         "/src/vm/src/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


