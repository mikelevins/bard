;;;; ***********************************************************************
;;;;
;;;; Name:          value-to-struct.scm
;;;; Project:       Bard
;;;; Purpose:       recovering the struct that represent's a value's type
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; getting bard types for values
;;; =====================================================================

(define (%value->struct val)
  (if (struct? val)
      (cond 
       ((primitive-struct? val) <primitive-struct>)
       ((structure-struct? val) <structure-struct>)
       ((base-struct? val) <base-struct>)
       ((record-struct? val) <record>)
       ((tuple-struct? val) <tuple>)
       ((union-struct? val) <union>)
       (else: (error (str "Unrecognized struct type: " val))))
      (if (struct-instance? val)
          (instance-struct val)
          (if (##structure? val)
              (%structure->struct (##structure-type val))
              (%tag->struct (%tag val))))))



