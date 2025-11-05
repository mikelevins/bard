;;;; ***********************************************************************
;;;;
;;;; Name:          fn.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       representing simple functions
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))
