;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.lisp
;;;; Project:       bard
;;;; Purpose:       loading and building Bard
;;;; Author:        mikel evins
;;;; Requirements:  Clozure Common Lisp
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

;;; ----------------------------------------------------------------------
;;; bard systems
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let* ((load-path (ccl::loading-file-source-file))
         (load-dir (make-pathname :directory (pathname-directory load-path)))) 
    (defun base-directory () load-dir)))

(defparameter $bard-src-files
  '("package"
    "runtime"
    "expressions"
    "types"
    "reader"
    "printer"
    "repl"
    ))

(defun compile-and-load (f)
  (with-compilation-unit ()
    (let ((path (make-pathname :name f :defaults (merge-pathnames "src/" (base-directory)))))
      (compile-file path)
      (load path))))

(defun load-bard ()
  (mapcar 'compile-and-load $bard-src-files))

(defun build-vm ()
  (load-bard)
  (save-application "bard" :prepend-kernel t))

