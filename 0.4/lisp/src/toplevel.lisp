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
  (let ((vm (make-instance '<vm> :mfn (compiler *bard-top-level*))))
    (vmrun vm)))

(defun comp-go (exp)
  (let ((vm (make-instance '<vm> :mfn (compiler `(exit ,exp)))))
    (vmrun vm)))

(defun comp-show (exp)
  (show  (compiler exp)))

