;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          callable.scm
;;;; Project:       Bard
;;;; Purpose:       callable objects
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; the common type of all callable objects

(define-type callable 
  extender: defcallable
  (name %debug-name))

;;; vm primitives

(defcallable prim
  constructor: %make-prim
  (required-arg-count %prim-required-arg-count)
  (restargs? %prim-restargs?)
  (function %prim-function))

;;; vm continuations

(defcallable continuation
  constructor: %private-make-continuation
  (fn %cc-fn)
  (env %cc-env)
  (stack %cc-stack)
  (pc %cc-pc))

(define (%makecc destpc)
  (%private-make-continuation "An anonymous continuation" (%fn) (%env) (%stack) destpc))

;;; fn: the common type of vm functions and methods
;;; values of type fn can be used as the value of the
;;; vm's %fn field. the code in a method executes the
;;; method's definition; the code in a function is a
;;; short routine that jumps to the vm's polymorphic
;;; function-dispatch code

(defcallable fn 
  extender: defn
  (code %fn-code %set-fn-code!))

(defn function
  constructor: %make-function
  (parameters %function-parameters))

(defn method
  constructor: %private-make-method
  (parameters %method-parameters)
  (env %method-env))
