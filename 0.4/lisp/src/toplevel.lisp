;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard toplevel
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter *bard-top-level*
  '(|begin| (|def| |bard|
             (|method| ()
              (|newline|)
              (|display| "bard> ")
              (|write| ((|compiler| (|read|))))
              (|bard|)))
    (|bard|)))

(defun bard ()
  (init-bard-comp)
  (vm (compiler *bard-top-level*)))

(defun comp-go (exp)
  (vm (compiler `(exit ,exp))))

