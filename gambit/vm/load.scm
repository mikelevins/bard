;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          load.scm
;;;; Project:       BardVM
;;;; Purpose:       system loader for VM development and testing
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the Bard sources are at another pathname

(define $bard-root "/Users/mikel/Projects/bard/gambit/vm/")

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define $load-files
  '("scm/bardvm.scm"
    ))

;;; load Scheme files
;;; ----------------------------------------------------------------------

(define (loadvm)
  (for-each (lambda (f)
              (let ((p (string-append $bard-root f)))
                (load p)))
            $load-files))

;;; (loadvm)
