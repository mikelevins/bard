;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.lisp
;;;; Project:       bard
;;;; Purpose:       toplevel repl for Bard
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun repl ()
  (let ((bard-prompt "bard>")
        (quit-sentinel ":quit"))
    (block bard-toplevel
      (loop
         (format t "~%~A " bard-prompt)
         (force-output)
         (let ((input (read-line)))
           (if (equalp input quit-sentinel)
               (return-from bard-toplevel
                 (progn
                   (format t "~%Terminating Bard toplevel...")
                   (force-output)))
               (progn
                 (format t "~%~S" (read input))
                 (force-output))))))
    (format t "~%")
    (force-output)))

