;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.el
;;;; Project:       bard: a kawa template project
;;;; Purpose:       emacs tools setup
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; setup site-specific variables
;;; ---------------------------------------------------------------------
;;; (load "/Users/mikel/Workshop/bard/0.4/emacs/bardc.el")

;;; find this file
(defvar $this-file-path nil)
(setq $this-file-path (or (symbol-file '$this-file-path)
			  load-file-name))

;;; init the emacs path
(defvar $bard-emacs-path (file-name-directory $this-file-path))

;;; deduce other project paths
(defvar $bard-root (expand-file-name (concat $bard-emacs-path "../")))
(defvar $bard-bin (expand-file-name (concat $bard-root "bin/")))
(defvar $bard-kawa-script (expand-file-name (concat $bard-bin "kawa")))

;;; adjust emacs
(add-to-list 'load-path (expand-file-name $bard-emacs-path))

;;; ---------------------------------------------------------------------
;;; Customizing Quack 
;;; ---------------------------------------------------------------------

;;; Whether to have a \"Quack\" menu always on the menu bar.
(set 'quack-global-menu-p t)

;;; Whether Quack should avoid use of Tab characters in indentation.
(set 'quack-tabs-are-evil-p t)

;;; Which font-lock fontification style to use. Options are plt,
;;; emacs, and nil
(set 'quack-fontify-style 'emacs)

;;; Whether three-semicolon comments should be fontified differently.
(set 'quack-fontify-threesemi-p nil)

;;; ---------------------------------------------------------------------
;;; run bard's Scheme 
;;; ---------------------------------------------------------------------

;;; run the bard kawa environment in quack mode
(defun kawa ()
  (interactive)
  (require 'quack)
  (let ((scheme-program-name $bard-kawa-script))
    (setenv "BARD_ROOT" $bard-root)
    (run-scheme scheme-program-name)))





