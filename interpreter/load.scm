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

(define $bard-root  "/Volumes/ymra/Users/mikel/Projects/bard/bard/interpreter/")

;;; termite includes and loads
;;; ----------------------------------------------------------------------

(##include "~~lib/gambit#.scm")
(##include "~~lib/_gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(load "~~/lib/termite/termite")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "lib/uuid.scm"
         "lib/srfi101.scm"
         "lib/wttree.scm"
         "lib/Sort.scm"
         "src/utils.scm"
         "src/values/type.scm"
         "src/values/function.scm"
         "src/values/type-functions.scm"
         "src/values/undefined.scm"
         "src/values/nothing.scm"
         "src/values/character.scm"
         "src/values/boolean.scm"
         "src/values/number.scm"
         "src/values/applicable.scm"
         "src/values/stream.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load (string-append $bard-root "/load.scm"))
;;; (load-bard)
