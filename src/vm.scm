;;;; ***********************************************************************
;;;;
;;;; Name:          vm.scm
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
;;; The bard vm creates and maintains one or more actors. An actor is
;;; an autonomous thread of control with its own stack and registers,
;;; its own code to execute, and its own mailbox.
;;;
;;; In the current version of bard 0.4, the number of actors in a vm
;;; is always exactly one, and send and receive are not implemented.
;;; Subsequent versions will add send and receive and the capability of
;;; running more than one actor per vm.

(define-structure vm
  globals
  actors)

(define (bardvm)
  (let* ((args (cdr (command-line)))
         (program-file (if (null? args)
                           #f
                           (car args))))
    (newline)
    (display $bard-version-string)
    (newline)))
