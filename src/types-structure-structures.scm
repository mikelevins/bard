;;;; ***********************************************************************
;;;;
;;;; Name:          types-structure-structures.scm
;;;; Project:       Bard
;;;; Purpose:       Bard structures that represent built-in Gambit structures
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; structure structures
;;; =====================================================================

(define <iostream> (make-structure-bard-structure '<iostream>
                                                  (%next-bard-type-number)
                                                  (##structure-type (current-input-port))))
(%register-structure-bard-structure! (structure-bard-structure-prototype <iostream>) <iostream>)

