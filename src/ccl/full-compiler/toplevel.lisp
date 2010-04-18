;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          toplevel.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       the initial toplevel environment
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD toplevel environment
;;; ============================================================

(defun init-initial-environment ()
  (init-modules)
  (let ((bard-module (find-module "bard"))
        (bard-env (bard-toplevel-environment)))
    ;; built-in primitive methods
    (intern "prim+" bard-module)
    (setf bard-env
          (add-binding bard-env
                       (bard::module-qualified-symbol (bard:symbol "bard.prim+"))
                       (lambda (arg0 arg1) 
                         (number
                          (+ (value arg0)
                             (value arg1))))))
    (intern "prim*" bard-module)
    (setf bard-env
          (add-binding bard-env
                       (bard::module-qualified-symbol (bard:symbol "bard.prim*"))
                       (lambda (arg0 arg1) 
                         (number
                          (* (value arg0)
                             (value arg1))))))
    (alter-toplevel-environment! bard-env)))
