;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       cesk: another experimental Bard VM
;;;; Purpose:       vm system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; project root
;;; ----------------------------------------------------------------------
;;; modify if the bard sources are at another pathname

(define $bard-root  "~/Projects/bard/vm/cesk")

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
         "/src/vm/src/utils.scm"
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
