;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       bard
;;;; Purpose:       system loader for development and testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root "/Users/mikel/Projects/bard/")

;;; termite includes and loads
;;; ----------------------------------------------------------------------

(##include "~~lib/gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")


;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/list-utils.scm"
         "src/string-utils.scm"
         "lib/wttree.scm"
         "lib/srfi101.scm"
         "src/values.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


