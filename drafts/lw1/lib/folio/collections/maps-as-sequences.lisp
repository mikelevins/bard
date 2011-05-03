;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional sequences
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :seq)

;;; =====================================================================
;;; maps as sequences
;;; =====================================================================

(defmethod length ((s map:ordered-map))(cl:length (entries s)))

