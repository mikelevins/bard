;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.el
;;;; Project:       Bard: a new lisp
;;;; Purpose:       emacs tools setup
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; setup site-specific variables
;;; ---------------------------------------------------------------------

;;;(load "/Users/mikel/Workshop/programming/bard/0.4/jvm/emacs/bard.el")

;;; find this file
(defvar $this-file-path nil)
(setq $this-file-path (or (symbol-file '$this-file-path)
			  load-file-name))

;;; init bard's emacs path
(defvar $bard-emacs-path (file-name-directory $this-file-path))

;;; deduce other project paths
(defvar $bard-root (expand-file-name (concat $bard-emacs-path "../")))
(defvar $bard-bin (expand-file-name (concat $bard-root "bin/")))
(defvar $bard-kawa-script (expand-file-name (concat $bard-bin "kawa")))

;;; adjust emacs
(add-to-list 'load-path (expand-file-name $bard-emacs-path))

;;; ---------------------------------------------------------------------
;;; run bard's Scheme 
;;; ---------------------------------------------------------------------

;;; run the bard Scheme environment in quack mode
(defun run-bard ()
  (interactive)
  (require 'quack)
  (let ((scheme-program-name $bard-kawa-script))
    (setenv "BARD_ROOT" $bard-root)
    (run-scheme scheme-program-name)))





