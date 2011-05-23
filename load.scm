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
(define $ymir-root "/Users/mikel/Projects/asgard/ymir/")

;;; termite includes and loads
;;; ----------------------------------------------------------------------

(##include "~~/lib/gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $vm-files
  (append
   (paths $ymir-root
          "src/utils/lists.scm"
          "src/utils/strings.scm")
   (paths $bard-root 
          "lib/wt-tree.scm"
          "lib/srfi101.scm"
          "src/cells.scm"
          "src/values.scm"
          "src/ast.scm"
          "src/reader.scm"
          )))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (load-vm)
  (for-each (lambda (f)(load f))
            $vm-files))

;;; (load-vm)


