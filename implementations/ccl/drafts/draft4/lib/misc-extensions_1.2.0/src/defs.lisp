;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CL-User -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :cl-user)

(defpackage :new-let
  (:use :cl)
  (:shadow cl:let cl:cond)
  (:export #:let #:cond #:nlet #:bcond))

(defpackage :gmap
  (:use :cl)
  (:export #:gmap #:def-gmap-arg-type #:def-gmap-res-type)
  (:shadowing-import-from :new-let #:let #:cond))

(defpackage :rev-fun-bind
  (:use :cl)
  (:export #:rlabels #:rflet #:rmacrolet))

(defpackage :lexical-contexts
  (:use cl)
  (:export #:defcontext #:with-context #:with-contexts #:deflex #:import-context
	   #:deflex-reinit #:isetq))

