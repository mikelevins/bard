;;;; ***********************************************************************
;;;;
;;;; Name:          load.scm
;;;; Project:       Bard
;;;; Purpose:       the system loader
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; modify if the bard sources are at another pathname

(define $bard-root  "/Users/mikel/Workshop/programming/bard/0.4") ; osx

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "/src/utils.scm"
         "/src/version.scm"
         "/src/error.scm"
         "/src/classes.scm"
         "/src/globals.scm"
         "/src/env.scm"
         "/src/values.scm"
         "/src/methods.scm"
         "/src/functions.scm"
         "/src/reader.scm"
         "/src/printer.scm"
         "/src/kernel.scm"
         "/src/shell.scm"
         "/src/quasiquote.scm"
         "/src/compiler-let.scm"
         "/src/compiler-loop.scm"
         "/src/compiler-set!.scm"
         "/src/compiler-define.scm"
         "/src/compiler-function.scm"
         "/src/compiler.scm"
         "/src/macros.scm"
         "/src/repl.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
;;; (globals:init)
;;; (bard:repl)


;;; TODO:
;;; - implement function dispatch (method calling is done)
;;; - implement class taxonomies
;;; - implement a way of marking functions and methods with a debug name
;;; - implement send and receive
;;; - implement protocols and the standard library
;;; - implement a mechanism for making foreign calls, for instance to a windowing subsystem

