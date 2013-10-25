;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          agent.lisp
;;;; Project:       Bard
;;;; Purpose:       representation of Bard agents
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an agent is an abstraction over a bard process. It is an object
;;; that represents a discrete session in a bard virtual machine. An
;;; agent has an associated compiler and vm, an image resource that
;;; was used to initialize it, and a message queue. 
;;;
;;; If running, an agent has an active session. Sessions (but not
;;; agents) can be serialized to images, and can be reconsituted by
;;; starting a vm and handing it the image, which the new vm
;;; reconstitutes by reading the image and transferring control to its
;;; toplevel procedure, thereby creating an agent. 
;;;
;;; Agents present a network interface that can be used to enqueue
;;; messages. No guarantee of delivery order is made, and messages may
;;; be processed in arbitrary order, or dropped on the floor.
;;;
;;; An agent may spawn other agents, in which case it immediately
;;; obtains references to them and can enqueue
;;; messages. Alternatively, an agent may receive a reference to
;;; another agent in a message sent by a third agent. 
;;;
;;; Communicating agents may share the same process, or may run in
;;; different processes on the same host, or may run on different
;;; hosts. Regardless, agents share no data and can communicate only
;;; through messages.
;;;
;;; A message may be any Bard data that can be serialized.

(defclass <agent> ()
  (compiler vm image messages))
