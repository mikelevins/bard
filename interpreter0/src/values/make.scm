;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          make.scm
;;;; Project:       Bard
;;;; Purpose:       the generic constructor
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ABOUT
;;; In this early version of Bard, there are no user-defined types.
;;; That being the case, make and as do not need to be extensible,
;;; and are therefore not generic. That will most likely change
;;; with the addition of user-defined types, and will also likely
;;; necessitate the addition of singletons to support eql specializers.

;;;---------------------------------------------------------------------
;;; API
;;;---------------------------------------------------------------------


