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

(define $bard-root  "/Users/mikel/Workshop/bard/0.4/core/") ; osx
;;;(define $bard-root  "/home/mikel/Projects/bard/interpreter/") ; Linux

;;; ----------------------------------------------------------------------
;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/version.scm"
         "lib/uuid.scm"
         "lib/Sort.scm"
         "src/utils.scm"
         "src/singleton-tree.scm"
         "src/types.scm"
         "src/types-primitive-types.scm"
         "src/types-classes.scm"
         "src/types-protocols.scm"
         "src/types-alist-table.scm"
         "src/type-signatures.scm"
         "src/types-function.scm"
         "src/types-interpreted-method.scm"
         "src/types-primitive.scm"
         "src/types-singleton.scm"
         "src/types-generator.scm"
         "src/types-structure-schemas.scm"
         "src/types-records.scm"
         "src/types-tuples.scm"
         "src/types-url.scm"
         "src/value-to-schema.scm"
         "src/env.scm"
         "src/primitives.scm"
         "src/read.scm"
         "src/print.scm"
         "src/special.scm"
         "src/macro.scm"
         "src/apply.scm"
         "src/eval.scm"
         "src/error.scm"
         "src/protocol-addressing.scm"
         "src/protocol-comparing.scm"
         "src/protocol-converting.scm"
         "src/protocol-creating.scm"
         "src/protocol-listing.scm"
         "src/protocol-mapping.scm"
         "src/protocol-ordering.scm"
         "src/protocol-pairing.scm"
         "src/protocol-streaming.scm"
         "src/protocol-text-processing.scm"
         "src/protocol-typing.scm"
         "src/init.scm"
         "src/bard.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load-bard)
;;; (%init-bard)
;;; (bard:repl)



