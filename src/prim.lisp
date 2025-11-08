;;;; ***********************************************************************
;;;;
;;;; Name:          prim.lisp
;;;; Project:       the bard programming lnaguage
;;;; Purpose:       representing primitive functions
;;;; Author:        mikel evins
;;;; Copyright:     2025 by mikel evins
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defstruct (prim (:type list))
  symbol n-args opcode always-true? side-effects)
