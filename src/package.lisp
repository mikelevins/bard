;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; package.lisp
;;;; definition of bard system packages
;;;; ---------------------------------------------------------------------

(defpackage #:bardvm
  (:use #:cl))


(defpackage #:bard
  (:use #:cl #:bardvm))
