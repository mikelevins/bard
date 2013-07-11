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

(define $bard-root  "/Users/mikel/Workshop/bard/0.4/") ; osx
;;;(define $bard-root  "/home/mikel/Projects/bard/interpreter/") ; Linux

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/vm.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (loadvm)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (loadvm)

;;; (make-stack size)
;;; (make-default-env)
;;; (make-default-globals)
;;; (make-default-prims)
;;; (load-program vm program)
;;; (instruction-opcode instr)
;;; (code-ref code index)
;;; (stack-push! v stack)
;;; (stack-pop! stack)
;;; (stack-top stack)
;;; (stack-take! n stack)
;;; (stack-adjoin! vals stack)
