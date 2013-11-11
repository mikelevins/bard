;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard toplevel
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; bard toplevel
;;; ---------------------------------------------------------------------

(defparameter *the-bard-vm* nil)

(defparameter *bard-top-level*
  (bard-read-from-string
   "
(begin 

 (def map
       (method (f ls)
               (if (<= (cons.length ls) 0)
                   nothing
                   (cons (f (cons.left ls))
                         (map f (cons.right ls))))))

 (def load
       (method (url)
               (map (^ (exp) ((compiler exp)))
                    (stream.read-all-objects
                     (stream.create url 'character)))))

 (def bard 
       (method () 
               (newline)
               (display \"bard> \")
               (write ((compiler (read))))
               (bard)))
 (if *init-file* (load *init-file*) nothing)
 (bard))

"))

(defun make-bardvm (&optional (toplevel-method *bard-top-level*))
  (make-instance '<vm> :mfn (compiler toplevel-method)))

(defun bard ()
  (setf *the-bard-vm* (make-bardvm))
  (let* ((init-path (probe-file "~/init.bard"))
         (init-url (if init-path
                       (url (format nil "file://~a" (namestring init-path)))
                       nil)))
    (def-global! *the-bard-vm* 'bard-symbols::|*init-file*| init-url))
  (vmrun *the-bard-vm*))

(defun bard-toplevel ()
  (format t "~%Bard version ~a~%~%" *bard-version-number*)
  (let ((vm (make-bardvm)))
    (handler-case (let* ((init-path (probe-file "~/init.bard"))
                         (init-url (if init-path
                                       (url (format nil "file://~a" (namestring init-path)))
                                       nil)))
                    (def-global! vm 'bard-symbols::|*init-file*| init-url)
                    (vmrun vm))
      (serious-condition (err)
        (format t "~%Unhandled error in the bard VM: ~S; terminating" err)
        (ccl::quit)))))

;;; ---------------------------------------------------------------------
;;; building bard
;;; ---------------------------------------------------------------------

(defun build-bard (path)
  (let* ((out-dir (format nil "~a/bard-~a/" path *bard-version-number*))
         (out-path (merge-pathnames "bard" out-dir)))
    (ensure-directories-exist out-dir)
    (ccl::save-application out-path :toplevel-function 'bard-toplevel :prepend-kernel t)))
