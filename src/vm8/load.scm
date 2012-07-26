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
         "/src/util/sort.scm"
         "/src/vm/util.scm"
         "/src/vm/constants.scm"
         "/src/vm/stretchy-vector.scm"
         "/src/vm/module.scm"
         "/src/vm/env.scm"
         "/src/vm/types.scm"
         "/src/vm/values.scm"
         "/src/vm/vm.scm"
         "/src/vm/op.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


