;;;; ***********************************************************************
;;;;
;;;; Name:          value-to-bard-structure.scm
;;;; Project:       Bard
;;;; Purpose:       recovering the bard-structure that represent's a value's type
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; getting bard types for values
;;; =====================================================================

(define (%value->structure val)
  (if (bard-structure? val)
      (cond 
       ((primitive-structure? val) <primitive-structure>)
       ((structure-bard-structure? val) <structure-structure>)
       ((base-bard-structure? val) <base-structure>)
       (else: (error (str "Unrecognized structure type: " val))))
      (if (bard-structure-instance? val)
          (instance-bard-structure val)
          (if (##structure? val)
              (%structure->bard-structure (##structure-type val))
              (%tag->bard-structure (%tag val))))))



