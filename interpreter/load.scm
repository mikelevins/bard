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

;;; Scheme files to load for interactive development
;;; ----------------------------------------------------------------------

(define (paths prefix . suffixes)
  (map (lambda (suffix)(string-append prefix suffix))
       suffixes))

(define $bard-files
  (paths $bard-root 
         "src/version.scm"
         "lib/uuid.scm"
         "lib/srfi101.scm"
         "lib/wttree.scm"
         "src/util/general.scm"
         "src/util/list_utils.scm"
         "src/util/sort.scm"
         "src/values/val_type.scm"
         "src/values/val_function.scm"
         "src/values/val_undefined.scm"
         "src/values/nothing.scm"
         "src/values/Val_character.scm"
         "src/values/Val_boolean.scm"
         "src/values/val_number.scm"
         "src/values/val_name.scm"
         "src/values/string.scm"
         "src/values/cons.scm"
         "src/values/primitive_procedure.scm"
         "src/values/val_frame.scm"
         "src/protocols/Anything.scm"
         "src/protocols/Type.scm"
         "src/protocols/Applicable.scm"
         "src/protocols/ForeignValue.scm"
         "src/protocols/StructureValue.scm"
         "src/protocols/PrimitiveValue.scm"
         "src/protocols/Undefined.scm"
         "src/protocols/List.scm"
         "src/protocols/Atom.scm"
         "src/protocols/Text.scm"
         "src/protocols/Frame.scm"
         "src/protocols/Procedure.scm"
         "src/protocols/Name.scm"
         "src/protocols/Null.scm"
         "src/protocols/Number.scm"
         "src/protocols/Character.scm"
         "src/protocols/Boolean.scm"
         "src/protocols/Method.scm"
         "src/protocols/Function.scm"
         "src/protocols/Keyword.scm"
         "src/protocols/Symbol.scm"
         "src/protocols/Float.scm"
         "src/protocols/Integer.scm"
         "src/protocols/Ratio.scm"
         "src/protocols/Comparable.scm"
         "src/print.scm"
         "src/protocols/IOStream.scm"
         "src/protocols/As.scm"
         "src/prims.scm"
         "src/eval/special.scm"
         "src/eval/macro.scm"
         "src/eval/apply.scm"
         "src/eval/env.scm"
         "src/eval/eval.scm"
         "src/reader/read.scm"
         "src/repl/error.scm"
         "src/repl/toplevel.scm"
         "nelson/Puzzle.scm"
         "src/bard.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (gc-report-set! #t)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load (string-append $bard-root "/load.scm"))
;;; (load-bard)
;;; (bard:repl)