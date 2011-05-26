;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          packages.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "CL-USER")

;;; ========================================================================
;;; Package definitions
;;; ========================================================================

(defpackage "BARD"
  (:use "COMMON-LISP" "CCL")
  (:shadow 
   "=" 
   "APPEND" "ASSOCIATE"
   "BOOLEAN"
   "CHARACTER" "CONCATENATE" "COUNT"
   "DISSOCIATE"
   "FALSE" "FIRST" "FLOAT"
   "GET"
   "INTEGER" "INTERN"
   "LAST"
   "MAP" "MERGE"
   "REST"
   "SECOND"
   "THIRD" "TRUE"))

;;; used for interning Bard names
(defpackage "BARD-NAMES")

