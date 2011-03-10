;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       bard entry points
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun run-batch (exp-string)
  (let* ((bard (init-bard))
         (expr (read-expr exp-string bard))
         (val (eval expr bard)))
    (format t "~%=> ~S~%" val)))

(defun repl (bard)
  (terpri)
  (block running
    (loop
       (format t "? ")
       (force-output t)
       (let ((inp (read-line)))
         (cond
           ((string= inp ":q")(return-from running))
           ((string= inp ":quit")(return-from running))
           (t (let* ((expr (read-expr inp bard))
                     (val (eval expr bard)))
                (format t "~%=> ~S~%" val))))))))

(defun run-repl ()
  (let* ((bard (init-bard)))
    (format t "~%")
    (format t "Bard v 1.0~%")
    (repl bard)))

(defun print-usage ()
  (format t "~%")
  (format t "Bard 1.0~%")
  (format t "~%")
  (format t "  USAGE: bard [expression]~%")
  (format t "~%"))