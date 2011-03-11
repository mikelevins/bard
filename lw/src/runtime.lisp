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

(defun %primitive (debug-name code &key (arg-count nil))
  (make-instance 'primitive :debug-name debug-name :code code :arg-count arg-count))

(defun init-modules ()
  (let* ((bard.prim (make-module "bard.prim"))
         (bard.core (make-module "bard.core"))
         (modules (fset:convert 'fset:map `(("bard.prim" . ,bard.prim)
                                            ("bard.core" . ,bard.core)))))
    (define-variable! bard.prim "+" :value (%primitive "+" #'%primitive-add) :export t)
    (define-variable! bard.prim "*" :value (%primitive "*" #'%primitive-multiply) :export t)
    (define-variable! bard.prim "-" :value (%primitive "-" #'%primitive-subtract) :export t)
    (define-variable! bard.prim "/" :value (%primitive "/" #'%primitive-divide) :export t)
    (define-variable! bard.core "*version*" :value (read-expr "\"1.0\"" nil) :export t)
    (define-variable! bard.core "*module*" :value bard.core :export t)
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