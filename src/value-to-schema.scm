;;;; ***********************************************************************
;;;;
;;;; Name:          value-to-schema.scm
;;;; Project:       Bard
;;;; Purpose:       recovering the schema that represent's a value's type
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; getting bard types for values
;;; =====================================================================

(define (%value->schema val)
  (if (schema? val)
      (cond 
       ((primitive-schema? val) <primitive-schema>)
       ((structure-schema? val) <structure-schema>)
       ((base-schema? val) <base-schema>)
       ((record-schema? val) <record>)
       ((tuple-schema? val) <tuple>)
       ((union-schema? val) <union>)
       (else: (error (str "Unrecognized schema type: " val))))
      (if (schema-instance? val)
          (instance-schema val)
          (if (##structure? val)
              (%structure->schema (##structure-type val))
              (%tag->schema (%tag val))))))



