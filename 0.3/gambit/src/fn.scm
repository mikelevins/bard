;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fn.scm
;;;; Project:       Bard
;;;; Purpose:       primitive bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define-type %fn
  constructor: %private-make-fn
  (name %fn-name)
  (parameters %fn-parameters)
  (env %fn-env)
  (code %fn-code))

(define (%makefn params code #!key (name "An anonymous primitive function")(env (%null-env)))
  (%private-make-fn name params env code))

