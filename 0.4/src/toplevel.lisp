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
             (write ((compiler (read))))
             (bard)))
 (bard))
"))

(defun bard ()
  (init-bard-comp)
  (handler-case (let ((vm (make-instance '<vm> :mfn (compiler *bard-top-level*))))
                  (vmrun vm))
    (serious-condition (err)
      (format t "~%Unhandled error in the bard VM: ~S; terminating" err)
      (ccl::quit))))

(defun comp-go (exp)
  (init-bard-comp)
  (let ((vm (make-instance '<vm> :mfn (compiler `(bard-symbols::|exit| ,exp)))))
    (vmrun vm)))

(defun comp-show (exp)
  (show  (compiler exp)))

(defun bard-toplevel ()
  (format t "~%Bard version ~a~%~%" *bard-version-number*)
  (bard))

(defun build-bard (path)
  (ccl::save-application path :toplevel-function 'bard-toplevel :prepend-kernel t))
