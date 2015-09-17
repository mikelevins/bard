;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.scm
;;;; Project:       bard 0.4
;;;; Purpose:       bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(module-export bard-repl)

(require "language.scm")

(define-private-alias Language gnu.expr.Language)

(define (bard-repl)
  (let* ((bard (bard-instance))
         (repl (kawa.repl bard))
         (args (String[])))
    (Language:registerLanguage (String[] "bard" "bard" ".bard" "BardLanguage"))
    (*:main repl args)))
