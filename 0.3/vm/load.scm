;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       bard VM system loader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root  "/Users/mikel/Projects/bard/0.3")

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/vm/version.scm"
         "/lib/uuid.scm"
         "/common/list-utils.scm"
         "/common/string-utils.scm"
         "/common/vector-utils.scm"
         "/vm/values.scm"
         "/vm/globals.scm"
         "/vm/environment.scm"
         "/vm/primitives.scm"
         ;; "/vm/op.scm"
         ;; "/vm/return-record.scm"
         ;; "/vm/instruction.scm"
         ;; "/vm/code.scm"
         ;; "/vm/applicable.scm"
         ;; "/vm/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)


