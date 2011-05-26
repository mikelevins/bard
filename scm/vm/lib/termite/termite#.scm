;; File: "termite#.scm"
;; Copyright (C) 2005-2008 Guillaume Germain

;; This is the declaration file for the Termite system
(##namespace ("termite#" 
			  ;; Termite "primordials"
			  self ! ? ?? !? on make-node spawn pid? 
			  spawn-link remote-spawn remote-spawn-link
			  ;; Useful
			  make-tag current-node
			  ;; Process linking for error propagation
			  inbound-link outbound-link full-link
			  ;; Wrap Gambit's I/O
			  spawn-output-port spawn-input-port
			  ;; Exceptions...
			  termite-exception? ;; actually that has to be exported for RECV ... :(
			  ;; Migration
			  migrate-task migrate/proxy 
			  ;; Useful condition reporting/logging procedures
			  warning debug info
			  ;; Node stuff
			  node-init node? node-host node-port 
			  ;; Nameserver mechanism
			  make-nameserver-node 
			  ;; OTP-style stuff (genserver)
			  make-server-plugin server:start server:start-link 
			  server:call server:cast server:stop 
			  ;; Distributed data structures
			  make-dict dict? dict->list dict-for-each dict-search 
			  dict-set! dict-ref dict-length
			  ;; Publishing and resolving names for services
			  publish-service unpublish-service resolve-service remote-service
			  ;; default init and node names for convenience
			  init node1 node2
			  *termite-nameserver-port*
			  *termite-cookie*
			  ;; Useful
			  ping
			  ))


(##define-macro (compile-time-load filename)
  (load filename)
  #f)

;; make it available at compile-time
(compile-time-load "~~/lib/termite/match-support.scm")

;; make it available at run-time
(##include "match-support.scm")


;; ----------------------------------------------------------------------------
;; Macros 

(include "match.scm")
(include "recv.scm")
(include "deftype.scm")
(include "uuid.scm")

