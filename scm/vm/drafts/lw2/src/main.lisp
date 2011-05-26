;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          main.lisp
;;;; Project:       Bard
;;;; Purpose:       main program
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun main ()
  (let ((args (cdr system:*line-arguments-list*)))
    (case (length args)
      ((0) (run-repl))
      ((1) (let ((inp (first args)))
             (run-batch inp)))
      (t (print-usage)))))