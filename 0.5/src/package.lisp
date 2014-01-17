;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          package.lisp
;;;; Project:       Bard
;;;; Purpose:       packages used by the bard compiler and VM
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
;;;;
;;;; ***********************************************************************

(defpackage #:bard
  (:use :cl)
  (:shadow #:find-symbol 
           #:intern 
           #:keyword
           #:map #:merge #:*modules* 
           #:symbol #:symbol-name
           #:unintern
           #:values))

;;; ---------------------------------------------------------------------
;;; bard packages
;;; ---------------------------------------------------------------------

(in-package :bard)



