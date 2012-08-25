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

(define $bard-root  "~/Projects/bard/vm")

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
         ;;"/src/utils.scm"
         ;;"/src/stretchy-vector.scm"
         ;;"/src/env.scm"
         ;;"/src/module.scm"
         ;;"/src/types.scm"
         ;;"/src/schema.scm"
         "/src/op.scm"
         "/src/vm.scm"
         ))

;;; global utils
;;; ----------------------------------------------------------------------

(define not-yet-implemented 
  (lambda args (error "function not yet implemented")))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
