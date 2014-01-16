;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.lisp
;;;; Project:       Bard
;;;; Purpose:       a Bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------

(defun bard ()
  (block bard
    (loop
       (format t "~%bard> ")
       (let ((token (bard-read)))
         (case token
           ((:|q| :|quit|) (terpri) (return-from bard 'done))
           (t (bard-print (bard-eval token))))))))
