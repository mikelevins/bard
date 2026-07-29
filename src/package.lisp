;;;; ***********************************************************************
;;;;
;;;; Name:          package.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       package definitions
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage :bard
  (:use #:cl)
  (:shadow #:disassemble)
  (:export
   ;; descriptors
   #:descriptor
   #:descriptor-name
   #:descriptor-call-handler
   #:descriptor-of
   ;; frames
   #:frame
   #:frame-parent
   #:frame-fn
   #:frame-pc
   #:frame-slots
   #:frame-sp
   #:make-frame
   #:frame-push
   #:frame-pop
   #:frame-top
   ;; code and functions
   #:code
   #:code-name
   #:code-instructions
   #:code-constants
   #:code-arity
   #:code-n-locals
   #:code-frame-size
   #:fn
   #:fn-code
   #:fn-captured-frame
   #:make-fn
   ;; bindings
   #:binding
   #:binding-name
   #:binding-value
   #:binding-bound?
   #:binding-dynamic?
   #:global-binding
   #:*globals*
   ;; primitives
   #:primitive
   #:primitive-name
   #:primitive-arity
   #:primitive-function
   #:install-primitive
   ;; threads
   #:thread
   #:thread-frame
   #:thread-status
   #:thread-dynenv
   #:make-thread
   ;; the machine
   #:run
   #:run-code
   #:*trace*
   #:bard-error
   #:bard-error-frame
   #:bard-error-pc
   ;; assembly and disassembly
   #:assemble
   #:disassemble
   #:instruction-string
   #:opcode-name
   #:*nothing*
   ))
