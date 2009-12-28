;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       the Bard compiler:
;;;;                ast -> Lisp code
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod compile ((exp bint::expression)(env bint::environment))
  (error "Unrecognized expression: ~s" exp))

