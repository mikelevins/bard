;; Copyright (C) 2005-2009 by Guillaume Germain, All Rights Reserved.
;; File: "termite.scm"

;; this is the main file for the Termite system
(##namespace ("termite#"))

(##include "~~/lib/gambit#.scm")
(##include "termite#.scm")

(declare
  (standard-bindings)
  (extended-bindings)
  (block))

;; ----------------------------------------------------------------------------
;; System configuration & global data

(define current-node (lambda () (error "uninitialized node")))

(define *global-mutex* (make-mutex "global termite mutex"))

;; translation tables for "published" PIDs
(define *foreign->local* (make-table weak-values: #t))
(define *local->foreign* (make-table weak-keys: #t))
;; translation table for "published" tags
(define *uuid->tag* (make-table weak-values: #t))

;; Get the current time in seconds.
(define (now)
  (time->seconds 
   (current-time)))

;; TODO Improve this
(define (formatted-current-time) 
  (let* ((port (open-process "date"))
		 (time (read-line port)))
	(close-port port)
	time))

;; ----------------------------------------------------------------------------
;; Datatypes

(define (process? obj) (thread? obj))
(define (process-links pid) (thread-specific pid))
(define (process-links-set! pid obj) (thread-specific-set! pid obj))

;; universal pid
(define-type upid
  id: 9e096e09-8c66-4058-bddb-e061f2209838
  tag
  node)

;; nodes
(define-type node
  id: 8992144e-4f3e-4ce4-9d01-077576f98bc5
  read-only:
  host
  port)

;; tags
(define-type tag
  id: efa4f5f8-c74c-465b-af93-720d44a08374
  (uuid init: #f))

;; * Test whether 'obj' is a pid.
(define (pid? obj)
  (or (process? obj) (upid? obj)))


;; NOTE It might be better to integrate with Gambit's exception mechanism
(define-type termite-exception
  id: 6a3a285f-02c4-49ac-b00a-aa57b1ad02cf
  origin
  reason
  object)


;; ----------------------------------------------------------------------------
;; process manipulation primitives

;; * Get the pid of the current process.
(define self current-thread)

;; Base exception handler for Termite processes.
(define (base-exception-handler e)
  (continuation-capture
    (lambda (k)
      (let ((log-crash 
              (lambda (e)
                (termite-log
                  'error
                  (list
                    (call-with-output-string ""
                      (lambda (port)
                        (display-exception-in-context e k port))))))))
        (cond
          ;; Propagated Termite exception?
          ((termite-exception? e)
           (if (not (eq? (termite-exception-reason e) 'normal))
               (log-crash (termite-exception-object e)))
           (for-each
             (lambda (pid) (! pid e))
             (process-links (self)))
           (halt!))
          ;; Gambit exception in the current process
          (else
            (log-crash e)
            (for-each
              (lambda (pid)
                (! pid (make-termite-exception (self) 'failure e)))
              (process-links (self)))
            (halt!)))))))


;; * Start a new process executing the code in 'thunk'.
(define (spawn thunk #!key (links '()) (name 'anonymous))
  (let ((t (make-thread
			 (lambda ()
			   (with-exception-handler
				 base-exception-handler
				 thunk)
			   (shutdown!))
             name)))
	(thread-specific-set! t links)
	(thread-start! t)
	t))


(define (spawn-linked-to to thunk #!key (name 'anonymous-linked-to))
  (spawn thunk links: (list to) name: name))


;; * Start a new process with a bidirectional link to the current
;; process.
(define (spawn-link thunk #!key (name 'anonymous-linked))
  (let ((pid (spawn thunk links: (list (self)) name: name)))
	(outbound-link pid)
	pid))


;; * Start a new process on remote node 'node', executing the code 
;; in 'thunk'.
(define (remote-spawn node thunk #!key (links '()) (name 'anonymous-remote))
  (if (equal? node (current-node))
	  (spawn thunk links: links name: name)
	  (!? (remote-service 'spawner node)
		  (list 'spawn thunk links name))))


;; * Start a new process on remote node 'node', with a bidirectional
;; link to the current process.
(define (remote-spawn-link node thunk)
  (let ((pid (remote-spawn node thunk links: (list (self)))))
	(outbound-link pid)
	pid))


;; * Cleanly stop the execution of the current process.  Linked
;; processes will receive a "normal" exit message.
(define (shutdown!)
  (for-each
	(lambda (pid)
	  (! pid (make-termite-exception (self) 'normal #f)))
	(process-links (self)))
  (halt!))

;; this is *not* nice: it wont propagate the exit message to the other
;; processes
(define (halt!)
  (thread-terminate! (current-thread)))


;; * Forcefully terminate a local process.  Warning: it only works on
;; local processes!  This should be used with caution. 
(define (terminate! victim)
  (thread-terminate! victim)
  (for-each
	(lambda (link)
	  (! link (make-termite-exception victim 'terminated #f)))
	(process-links victim)))


;; TODO 'wait-for' and 'alive?' should be grouped in a more general
;; procedure able to determine the status of a process (alive, dead,
;; waiting, etc.) and the procedure should work on remote processes

;; * Wait for the end of a process 'pid'.  Does not return anything.
;; Warning: will not work on remote processes.
(define (%wait-for pid)
  (with-exception-catcher
	(lambda (e)
	  (void))
	(lambda ()
	  (thread-join! pid)
	  (void))))


;; Check whether the process 'pid' is still alive.  Warning: will not
;; work on remote processes.
(define (%alive? pid)
  (with-exception-catcher
	(lambda (e)
	  (join-timeout-exception? e))
	(lambda ()
	  (thread-join! pid 0)
	  #f)))


;; ----------------------------------------------------------------------------
;; Sending messages

;; * Send a message 'msg' to 'pid'.  This means that the message will
;; be enqueued in the mailbox of the destination process.
;; 
;; Delivery of the message is unreliable in theory, but in practice
;; local messages will always be delivered, and remote messages will
;; not be delivered only if the connection is currently broken to the
;; remote node, or if the remote node is down.
;;
;; Note that you will not get an error or an exception if the message
;; doesn't get there: you need to handle errors yourself.
(define (! to msg)
  (cond
	((process? to)
	 (thread-send to msg))
	((upid? to)
	 (thread-send dispatcher (list 'relay to msg)))
	(else
	  (error "invalid-message-destination" to))))


;; ----------------------------------------------------------------------------
;; Receiving messages

;; incorrect, because it doesn't handle exception messages
;; (define ? thread-receive)

;; * Retrieve the first message from the mailbox of the current
;; process.  If no message is available, the process will block until
;; a message is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
(define (? . opt) ;; TODO: inefficient, fix
  (match opt
	(()
	 (recv
	   (msg msg)))

	((timeout)
	 (recv
	   (msg msg)
	   (after timeout (thread-receive 0))))

	((timeout default)
	 (recv
	   (msg msg)
	   (after timeout default)))))


;; benchmark to see if faster...
;; (define (? #!optional (timeout +inf.0) (default (lambda (thread-receive 0))))
;;   (with-exception-catcher
;;    (lambda (exception)
;;      (if (mailbox-receive-timeout-exception? exception)
;;          (default)
;;          (raise exception)))
;;    (lambda ()
;;      (thread-receive timeout))))


;; * Retrieve the first message from the mailbox of the current
;; process that satisfised the predicate 'pred?'.  If no message
;; qualifies, the process will block until a message satisfying the
;; predicate is received.  If 'timeout' is specified, the process will
;; only block for that amount of time, and then raise an exception.
;; It is possible to also pass the 'default' argument to return a
;; value instead of raising an exception.
;; TODO: inefficient, fix
(define (?? pred? . opt)
  (match opt
	(()
	 (recv
	   (msg (where (pred? msg)) msg)))

	((timeout)
	 (recv
	   (msg (where (pred? msg)) msg)
	   (after timeout (thread-receive 0))))

	((timeout default)
	 (recv
	   (msg (where (pred? msg)) msg)
	   (after timeout default)))))


;; ----------------------------------------------------------------------------
;; Higher-order concurrency primitives

;; * Send a "synchronous" message to a process.  The message will be
;; annotated with a tag and the pid of the current process, therefore
;; sending a message of the form '(from tag msg)'.  The server
;; receiving the message must specifically handle that format of
;; message, and reply with a message of the form '(tag reply)'.
;;
;; Like for the |?| and |??| message retrieving operators, it is
;; possible to specify a 'timeout' to limit the amount of time to wait
;; for a reply, and a 'default' value to return if no reply has been
;; received.
;; RPC
(define (!? pid msg . opt)
  (let ((tag (make-tag)))
	(! pid (list (self) tag msg))

	(match opt
	  (()
	   (recv
		 ((,tag reply) reply)))

	  ((timeout)
	   (recv
		 ((,tag reply) reply)
		 (after timeout (raise 'timeout))))

	  ((timeout default)
	   (recv
		 ((,tag reply) reply)
		 (after timeout default))))))


;; * Evaluate a 'thunk' on a remote node and return the result of that
;; evaluation.  Just like for |!?|, |?| and |??|, it is possible to
;; specify a 'timeout' and a 'default' argument.
(define (on node thunk)
  (let ((tag (make-tag))
		(from (self)))
	(remote-spawn node
				  (lambda ()
					(! from (list tag (thunk)))))
	(recv
	  ((,tag reply) reply))))


;; ----------------------------------------------------------------------------
;; Links and exception handling

;; Default callback for received exceptions.
(define (handle-exception-message event)
  (raise event))

;; * Link another process 'pid' /to/ the current one: any exception
;; not being caught by the remote process and making it crash will be
;; propagated to the current process.
(define (inbound-link pid)
  (! linker (list 'link pid (self))))


;; * Link the current process /to/ another process 'pid': any
;; exception not being caught by the current process will be
;; propagated to the remote process.
(define (outbound-link pid)
  (let* ((links (process-links (self))))
	(if (not (memq pid links))
		(process-links-set! (self) (cons pid links)))))


;; * Link bidirectionally the current process with another process
;; 'pid': any exception not being caught in any of the two processes
;; will be propagated to the other one.
(define (full-link pid)
  (inbound-link  pid)
  (outbound-link pid))


;; ----------------------------------------------------------------------------
;; Termite I/O

;; Wraps 'pid's representing Gambit output ports.
(define-type termite-output-port
  id: b0c30401-474c-4e83-94b4-d516e00fe363
  unprintable:
  pid)

;; Wraps 'pid's representing Gambit input ports.
(define-type termite-input-port
  id: ebb22fcb-ca61-4765-9896-49e6716471c3
  unprintable:
  pid)

;; Start a process representing a Gambit output port.
(define (spawn-output-port port #!optional (serialize? #f))
  (output-port-readtable-set!
	port
	(readtable-sharing-allowed?-set
	  (output-port-readtable port)
	  serialize?))

  (make-termite-output-port
	(spawn
	  (lambda ()
		(let loop ()
		  (recv
			(proc
			  (where (procedure? proc))
			  (proc port))
			(x (warning "unknown message sent to output port: " x)))
		  (loop)))
      name: 'termite-output-port)))

;; Start a process representing a Gambit input port.
(define (spawn-input-port port #!optional (serialize? #f))
  (input-port-readtable-set!
	port
	(readtable-sharing-allowed?-set
	  (input-port-readtable port)
	  serialize?))

  (make-termite-input-port
	(spawn
	  (lambda ()
		(let loop ()
		  (recv
			((from token proc)
			 (where (procedure? proc))
			 (! from (list token (proc port))))
			(x (warning "unknown message sent to input port: " x)))
		  (loop)))
      name: 'termite-input-port)))

;; IO parameterization
;; (define current-termite-input-port (make-parameter #f))
;; (define current-termite-output-port (make-parameter #f))

;; insert IO overrides
;; (include "termiteio.scm")


;; ----------------------------------------------------------------------------
;; Distribution

;; Convert a 'pid'
(define (pid->upid obj)
  (mutex-lock! *global-mutex*)
  (cond
	((table-ref *local->foreign* obj #f)
	 => (lambda (x)
		  (mutex-unlock! *global-mutex*)
		  x))
	(else
	  (let ((upid (make-upid (make-uuid) (current-node))))
		(table-set! *local->foreign* obj upid)
		(table-set! *foreign->local* upid obj)
		(mutex-unlock! *global-mutex*)
		upid))))

(define (tag->utag obj)
  (mutex-lock! *global-mutex*)
  (cond
	((tag-uuid obj)
	 (mutex-unlock! *global-mutex*)
	 obj)
	(else
	  (let ((uuid (make-uuid)))
		(tag-uuid-set! obj uuid)
		(table-set! *uuid->tag* uuid obj)
		(mutex-unlock! *global-mutex*)
		obj))))


(define (serialize-hook obj)
  (cond
	((process? obj)
	 (pid->upid obj))

	((tag? obj) 
	 (tag->utag obj))

	;; unserializable objects, so instead of crashing we set them to #f
	((or (port? obj)) 
	 #f)

	(else obj)))

(define (upid->pid obj)
  (cond
	((table-ref *foreign->local* obj #f)
	 => (lambda (pid) pid))
	((and (symbol? (upid-tag obj))
		  (resolve-service (upid-tag obj)))
	 => (lambda (pid) 
		  pid))
	(else
	  (error "don't know how to upid->pid"))))

(define (utag->tag obj)
  (let ((uuid (tag-uuid obj)))
	(cond
	  ((table-ref *uuid->tag* uuid #f)
	   => (lambda (tag) tag))
	  (else obj))))

(define (deserialize-hook obj)
  (cond
	((and (upid? obj)
		  (equal? (upid-node obj)
				  (current-node)))
	 (upid->pid obj))
	((tag? obj)
	 (utag->tag obj))
	(else obj)))


(define (serialize obj port)
  (let* ((serialized-obj
		   (object->u8vector obj serialize-hook))
		 (len
		   (u8vector-length serialized-obj))
		 (serialized-len
		   (u8vector (bitwise-and len #xff)
					 (bitwise-and (arithmetic-shift len -8) #xff)
					 (bitwise-and (arithmetic-shift len -16) #xff)
					 (bitwise-and (arithmetic-shift len -24) #xff))))

	(begin
	  (write-subu8vector serialized-len 0 4 port)
	  (write-subu8vector serialized-obj 0 len port))))


(define (deserialize port)
  (let* ((serialized-len
		   (u8vector 0 0 0 0))
		 (n
		   (read-subu8vector serialized-len 0 4 port)))

	(cond ((= 0 n)
		   #!eof)
		  ((not (= 4 n))
		   (error "deserialization error"))
		  (else
			(let* ((len
					 (+ (u8vector-ref serialized-len 0)
						(arithmetic-shift (u8vector-ref serialized-len 1) 8)
						(arithmetic-shift (u8vector-ref serialized-len 2) 16)
						(arithmetic-shift (u8vector-ref serialized-len 3) 24)))
				   (serialized-obj
					 (make-u8vector len))
				   (n
					 (read-subu8vector serialized-obj 0 len port)))

			  (if (not (eqv? len n))
				  (begin
					(error "deserialization error"
						   (list len: len n: n)))
				  (let ((obj (u8vector->object serialized-obj deserialize-hook)))
					(if (vector? obj)
						(vector->list obj)
						obj))))))))

(define (start-serializing-output-port port)
  (spawn-link
	(lambda ()
	  (let loop ()
		(recv
		  (('write data)
		   ;; (debug out: data)
		   (serialize data port)
		   (force-output port)) ;; io override

		  (msg
			(warning "serializing-output-port ignored message: " msg)))
		(loop)))
    name: 'termite-serializing-output-port))


(define (start-serializing-active-input-port port receiver)
  (spawn-link
    (lambda ()
      (let loop ()
        (let ((data (deserialize port)))
          ;; to receive exceptions...
          (? 0 'ok)
          ;; (debug in: data)
          (if (eof-object? data) (shutdown!))
          (! receiver (list (self) data))
          (loop))))
    name: 'termite-serializing-active-input-port))


;; a tcp server listens on a certain port for new tcp connection
;; requests, and call ON-CONNECT to deal with those new connections.
(define (start-tcp-server tcp-port-number on-connect)
  (let ((tcp-server-port
		  (open-tcp-server (list
							 port-number: tcp-port-number
							 coalesce: #f))))
	(spawn
	  (lambda ()
		(let loop () 
		  (on-connect (read tcp-server-port)) ;; io override
		  (loop)))
      name: 'termite-tcp-server)))


;; MESSENGERs act as proxies for sockets to other nodes

;; initiate a new bidirectional connection to another node important:
;; caller is responsible for registering it with the dispatcher
(define (initiate-messenger node)
  ;; (print "OUTBOUND connection established\n")
  (spawn
	(lambda ()
	  (with-exception-catcher
		(lambda (e)
		  (! dispatcher (list 'unregister (self)))
		  (shutdown!))

		(lambda ()
		  (let ((socket (open-tcp-client
						  (list server-address: (node-host node)
								port-number:    (node-port node)
								coalesce:       #f))))
			;; the real interesting part
			(let ((in  (start-serializing-active-input-port socket (self)))
				  (out (start-serializing-output-port socket)))

			  (! out (list 'write (current-node)))

			  (messenger-loop node in out))))))
    name: 'termite-outbound-messenger))


;; start a MESSENGER for an 'inbound' connection (another node
;; initiated the bidirectional connection, see initiate-messenger)
(define (start-messenger socket)
  ;; (print "INBOUND connection established\n")
  (spawn
	(lambda ()
	  (with-exception-catcher
		(lambda (e)
		  (! dispatcher (list 'unregister (self)))
		  (shutdown!))

		(lambda ()
		  (let ((in  (start-serializing-active-input-port socket (self)))
				(out (start-serializing-output-port socket)))
			(recv
			  ((,in node)
			   ;; registering messenger to local dispatcher
			   (! dispatcher (list 'register (self) node))
			   (messenger-loop node in out)))))))
    name: 'termite-inbound-messenger))


(define (messenger-loop node in out)
  (recv
	;; incoming message
	((,in ('relay id message))
	 (let ((to (upid->pid (make-upid id (current-node)))))
	   (! to message)))

	;; outgoing message
	(('relay to message)
	 ;; 'to' is a upid
	 (let* ((id (upid-tag to))
			;; (node (upid-node to))
			;; (host (node-host node))
			;; (port (node-id node))
			)
	   (! out (list 'write (list 'relay id message)))))

	;; unknown message
	(msg
	  (warning "messenger-loop ignored message: " msg)))

  (messenger-loop node in out))


;; the DISPATCHER dispatches messages to the right MESSENGER, it keeps
;; track of known remote nodes
(define dispatcher
  (spawn 
	(lambda () 
	  ;; the KNOWN-NODES of the DISPATCHER LOOP is an a-list of NODE => MESSENGER
	  (let loop ((known-nodes '()))
		(recv
		  (('register messenger node)
		   (loop (cons (cons node messenger) known-nodes)))

		  (('unregister messenger)
		   (loop (remove (lambda (m) (equal? (cdr m) messenger)) known-nodes)))

		  (('relay upid message)
		   (let ((node (upid-node upid)))
			 (cond
			   ;; the message should be sent locally (ideally should not happen 
			   ;; for performance reasons, but if the programmer wants to do 
			   ;; that, then OK...)
			   ((equal? node (current-node))
				(! (upid->pid upid) message)
				(loop known-nodes))

			   ;; the message is destined to a pid on a known node
			   ((assoc node known-nodes)
				=> (lambda (messenger)
					 (! (cdr messenger) (list 'relay upid message))
					 (loop known-nodes)))

			   ;; unconnected node, must connect
			   (else
				 (let ((messenger (initiate-messenger node)))
				   (! messenger (list 'relay upid message))
				   (loop (cons (cons node messenger) known-nodes)))))))

		  (msg
			(warning "dispatcher ignored message: " msg) ;; uh...
			(loop known-nodes)))))
    name: 'termite-dispatcher))


;; ----------------------------------------------------------------------------
;; Services

;; LINKER (to establish exception-propagation links between processes)
(define linker
  (spawn
	(lambda ()
	  (let loop ()
		(recv
		  (('link from to)
		   (cond
			 ((process? from)
			  (process-links-set! from (cons to (process-links from)))) ;;;;;;;;;;
			 ((upid? from)
			  (! (remote-service 'linker (upid-node from))
				 (list 'link from to)))
			 (else
			   (warning "in linker-loop: unknown object"))))
		  (msg
			(warning "linker ignored message: " msg)))
		(loop)))
    name: 'termite-linker))


;; Remote spawning
;; the SPAWNER answers remote-spawn request
(define spawner
  (spawn
	(lambda ()
	  (let loop ()
		(recv
		  ((from tag ('spawn thunk links name))
		   (! from (list tag (spawn thunk links: links name: name))))

		  (msg
			(warning "spawner ignored message: " msg)))
		(loop)))
    name: 'termite-spawner))


;; the PUBLISHER is used to implement a mutable global env. for
;; process names
(define publisher
  (spawn 
	(lambda ()
	  (define dict (make-dict))

	  (let loop ()
		(recv
		  (('publish name pid)
		   (dict-set! dict name pid))

		  (('unpublish name pid)
		   (dict-set! dict name))

		  ((from tag ('resolve name))
		   (! from (list tag (dict-ref dict name))))

		  (msg
			(warning "puslisher ignored message: " msg)))

		(loop)))
    name: 'termite-publisher))

(define (publish-service name pid)
  (! publisher (list 'publish name pid)))

(define (unpublish-service name pid)
  (! publisher (list 'unpublish name pid)))

;; This should probably only used internally
(define (resolve-service name #!optional host)
  (!? publisher (list 'resolve name)))

;; * Get the pid of a service on a remote node 'node' which has been
;; published with |publish-service| to the name 'service-name'.
(define (remote-service service-name node)
  (make-upid service-name node))


;; ----------------------------------------------------------------------------
;; Erlang/OTP-like behavior for "generic servers" and "event handlers"

(include "otp/gen_server.scm")
(include "otp/gen_event.scm")


;; ----------------------------------------------------------------------------
;; Some datastrutures

(include "data.scm")


;; ----------------------------------------------------------------------------
;; Migration

;; Task moves away, lose identity
(define (migrate-task node)
  (call/cc
	(lambda (k)
	  (remote-spawn node (lambda () (k #t)))
	  (halt!))))

;; Task moves away, leave a proxy behind
(define (migrate/proxy node)
  (define (proxy pid)
	(let loop ()
	  (! pid (?))
	  (loop)))
  (call/cc
	(lambda (k)
	  (proxy
		(remote-spawn-link node (lambda () (k #t)))))))


;; ----------------------------------------------------------------------------
;; A logging facility for Termite

;; (Ideally, this should be included with the services, but the
;; writing style is much different.  Eventually, the other services
;; might use similar style.)

(define (report-event event port)
  (match event
    ((type who messages)
     (with-output-to-port port
       (lambda ()
         (newline)
         (display "[")
         (display type)
         (display "] ")
         (display (formatted-current-time))
         (newline)
         (display who)
         (newline)
         (for-each (lambda (m) (display m) (newline)) messages)
         (force-output))))
    (_ (display "catch-all rule invoked in reporte-event")))
  port)

(define file-output-log-handler
  (make-event-handler
	;; init
	(lambda (args)
	  (match args
		((filename) 
         (open-output-file (list path: filename
                                 create: 'maybe
                                 append: #t)))))
	;; event
	report-event
	;; call
	(lambda (term port)
	  (values (void) port))
	;; shutdown
	(lambda (reason port)
	  (close-output-port port))))


;; 'type' is a keyword (error warning info debug)
(define (termite-log type message-list)
  (event-manager:notify logger (list type (self) message-list)))

(define (warning . terms)
  (termite-log 'warning terms))

(define (info . terms)
  (termite-log 'info terms))

(define (debug . terms)
  (termite-log 'debug terms))

(define logger 
  (let ((logger (event-manager:start name: 'termite-logger)))
	(event-manager:add-handler logger
							   (make-simple-event-handler
								 report-event
								 (current-error-port)))
	(event-manager:add-handler logger
							   file-output-log-handler
							   "_termite.log")
	logger))


(define ping-server
  (spawn 
	(lambda ()
	  (let loop ()
		(recv
		  ((from tag 'ping) 
		   (! from (list tag 'pong)))
		  (msg (debug "ping-server ignored message" msg)))
		(loop)))
    name: 'termite-ping-server))

(define (ping node #!optional (timeout 1.0))
  (!? (remote-service 'ping-server node) 'ping timeout 'no-reply))

;; ----------------------------------------------------------------------------
;; Initialization

(process-links-set! (self) '())

(define (node-init node)
  (start-tcp-server (node-port node) start-messenger)
  (set! current-node (lambda () node))
  (publish-external-services)
  'ok)

(define (publish-external-services)
  ;; --------------------
  ;; Services
  
  ;; publishing the accessible exterior services
  ;; (essentially, opening the node to other nodes)
  (publish-service 'spawner spawner)
  (publish-service 'linker linker)
  (publish-service 'ping-server ping-server))


;; Some convenient definitions
(define node1 (make-node "localhost" 3001))
(define node2 (make-node "localhost" 3002))

