;;;; ***********************************************************************
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       the bard vm
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; The bard vm creates and maintains one or more engines. An engine is
;;; an autonomous thread of control with its own stack and registers,
;;; its own code to execute, and its own mailbox.

(define *bardvm*
  )

(define (bardvm)
  #f)

(bardvm)





