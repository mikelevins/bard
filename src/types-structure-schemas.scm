;;;; ***********************************************************************
;;;;
;;;; Name:          types-structure-schemas.scm
;;;; Project:       Bard
;;;; Purpose:       representing built-in Gambit structure types
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; structure schemas
;;; =====================================================================

(define <iostream> (make-structure-schema '<iostream> (%next-bard-type-number) (##structure-type (current-input-port))))
(%register-structure-schema! (structure-schema-prototype <iostream>) <iostream>)

