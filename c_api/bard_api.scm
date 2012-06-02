;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard_api.scm
;;;; Project:       Bard
;;;; Purpose:       Scheme API functions
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; This file defines Scheme API functions that are called by the C API
;;; functions defined as c-lambdas in bard_c_api.scm

(define (api:version)
  (objc:string->NSString $bard-version-string))


(define (api:list-files dirpath)
  (display dirpath))
