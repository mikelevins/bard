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
         "src/util/general.scm"
         "src/util/list.scm"
         "src/util/sort.scm"
         "src/values/type.scm"
         "src/values/function.scm"
         "src/values/undefined.scm"
         "src/values/nothing.scm"
         "src/values/character.scm"
         "src/values/boolean.scm"
         "src/values/number.scm"
         "src/values/name.scm"
         "src/values/string.scm"
         "src/values/cons.scm"
         "src/values/primitive-procedure.scm"
         "src/values/frame.scm"
         "src/protocols/Anything.scm"
         "src/protocols/Apply.scm"
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
         "src/protocols/Equal.scm"
         "src/protocols/Compare.scm"
         ;;"src/values/stream.scm"
         "src/prims.scm"
         "src/eval/special.scm"
         "src/eval/macro.scm"
         "src/eval/apply.scm"
         "src/eval/env.scm"
         "src/eval/eval.scm"
         "src/print.scm"
         "src/read.scm"
         "src/repl/repl.scm"
         ))

;;; load sources
;;; ----------------------------------------------------------------------

(define (load-bard)
  (for-each (lambda (f)(load f))
            $bard-files))

;;; (load (string-append $bard-root "/load.scm"))
;;; (load-bard)
