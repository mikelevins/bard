;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       bardvm system loader
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bardvm-root "/Users/mikel/Workshop/src/bard/vm/") ; macos


(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bardvm-files
  (paths $bardvm-root 
         "version.scm"
         "read.scm"
         "repl.scm"
         "bardvm.scm"
         ))


(define (load-bardvm)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bardvm-files))

;;; (load-bardvm)
;;; (bardvm:repl)
