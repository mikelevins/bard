;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       Bard
;;;; Purpose:       bard runtime
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun init-modules ()
  (let* ((bard.core (make-module "bard.core" 
                                 :exports '("version")
                                 :variables `(("*version*" ,(read-expr "\"1.0\"" nil))
                                              ("*module*"))))
         (modules (fset:convert 'fset:map `(("bard.core" . ,bard.core)))))
    (set-module-variable! bard.core "*module*" bard.core)
    modules))

(defclass bard-runtime ()
  ((modules :reader modules :initform (init-modules))))

(defun init-bard ()(make-instance 'bard-runtime))

;;; (setq $bard (init-bard))

(defmethod find-module ((bard bard-runtime) (mname string))
  (fset:@ (modules bard) mname))

(defmethod find-module ((bard bard-runtime) (mname null))
  (fset:@ (modules bard) "bard.core"))

(defmethod current-module-name ((bard bard-runtime))
  "bard.core")

(defparameter *bard-character-name-table*
  `(("space" . #\space)
    ("newline" . #\newline)
    ("tab" . #\tab)
    ))

(defmethod print-name-for (thing)(format nil "~S" thing))
(defmethod print-name-for ((thing cl:character))
  (let ((chstr (format nil "~S" thing)))
    (subseq chstr 2)))

(defmethod character-data-for-name (charname)(error "Unknown character name: ~A" charname))
(defmethod character-data-for-name ((charname string))
  (let ((entry (assoc charname *bard-character-name-table* :test #'equalp)))
    (and entry (cdr entry))))

(defmethod find-character-data (charname)(character-data-for-name charname))
(defmethod find-character-data ((ch cl:character)) ch)
(defmethod find-character-data ((charname string))
  (if (= (length charname) 1)
      (find-character-data (elt charname 0))
      (character-data-for-name charname)))