;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          lib.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       library APIs for Bard base types
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; Library APIs
;;; ============================================================

;;; booleans
;;; numbers
;;; sequences

(defmethod count ((x fset:seq))
  (fset:size x))

(defmethod count ((x fset:map))
  (fset:size x))

(defun element (s n)
  (fset:@ s n))

(defmethod map-over ((f function)(s fset:seq))
  (fset:image f s))

;;; text
;;; map

