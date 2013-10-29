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
  (bard-read-from-string
   "
(begin 
 (set! bard 
     (method () 
             (newline)
             (display \"bard> \")
             (write (compiler (read)))
             (bard)))
 (bard))
"))

(defun bard ()
  (init-bard-comp)
  (vmrun (compiler *bard-top-level*)))

(defun comp-go (exp)
  (vmrun (compiler `(exit ,exp))))

(defun comp-show (exp)
  (show  (compiler exp)))

