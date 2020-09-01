;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; builtins.lisp
;;;; variables and operations built into the initial toplevel environment
;;;; ---------------------------------------------------------------------

(in-package :bardvm)

;;; ---------------------------------------------------------------------
;;; lists
;;; ---------------------------------------------------------------------

;;; each fn list
;;; ---------------------------------------------------------------------
;;; returns a list of values returned by applying fn to each element
;;; of list. like Lisp's mapcar

;;; ---------------------------------------------------------------------
;;; maps
;;; ---------------------------------------------------------------------

