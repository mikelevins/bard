;;;; ***********************************************************************
;;;;
;;;; Name:          types-structure-structs.scm
;;;; Project:       Bard
;;;; Purpose:       representing built-in Gambit structure types
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; structure structs
;;; =====================================================================

(define <iostream> (make-structure-struct '<iostream> (%next-bard-type-number) (##structure-type (current-input-port))))
(%register-structure-struct! (structure-struct-prototype <iostream>) <iostream>)

