;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; Kawa-Scheme implementation of thread-ring benchmark.
;;; Contributed by Per Bothner
;;; Based on Java 6 -server #4 version contributed by Fabien Le Floc'h
;;; Best performance is achieved with
;;; MAX_THREAD=1 as the thread-ring test is bested with only 1 os thread.
;;; This implementation shows using a simple thread pool solves the
;;; thread context switch issue.


(define n :: int (string->number (cadr (command-line))))
(define m :: int (if (null? (cddr (command-line))) 503
                     (string->number (caddr (command-line)))))

(define-constant MAX_NODES :: int m)
(define-constant MAX_THREADS :: int m)

(define-class TokenMessage ()
  (node-id :: int)
  (value :: int access: 'volatile)
  (is-stop :: boolean)
  ((*init* (node-id :: int) (value :: int))
   (set! (this):node-id node-id)
   (set! (this):value value))
  ((*init* (node-id :: int) (value :: int) (is-stop :: boolean))
   (set! (this):node-id node-id)
   (set! (this):value value)
   (set! (this):is-stop is-stop)))

(define-class Node (java.lang.Runnable)
  (node-id :: int)
  (next-node :: Node)
  (queue :: java.util.concurrent.BlockingQueue
	 init: (java.util.concurrent.LinkedBlockingQueue))
  (is-active :: boolean)
  (counter :: int)

  ((*init* (id :: int))
   (set! (this):node-id id))

  #|
((*init* (node :: Node))
   (set! (this):next-node node)
   (set! is-active #t))
|#
  ((connect (node :: Node)) :: void
   (set! next-node node)
   (set! is-active #t))

  ((send-message (m :: TokenMessage)) :: void
   (queue:add m)
   (executor:execute (this)))

  ((run) :: void
   (if is-active
       (try-catch
	(let ((m :: TokenMessage (queue:take)))
	  (if m:is-stop
	      (let ((next-value (+ 1 m:value)))
		(cond ((= next-value MAX_NODES)
		       (executor:shutdown))
		      (else
		       (set! m:value next-value)
		       (next-node:send-message m)))
		(set! is-active #f))
	      (cond ((= m:value n)
		     (java.lang.System:out:println node-id)
		     (next-node:send-message (TokenMessage node-id 0 #t)))
		    (else
		     (set! m:value (+ m:value 1))
		     (next-node:send-message m)))))
	(ex java.lang.InterruptedException
	    (ex:printStackTrace)))))
)

(define executor :: java.util.concurrent.ExecutorService
  (java.util.concurrent.Executors:newFixedThreadPool MAX_THREADS))

(define nodes :: Node[] (Node[] length: (+ MAX_NODES 1)))

(do ((i :: int 0 (+ i 1))) ((>= i MAX_NODES))
  (set! (nodes i) (Node (+ i 1))))
(set! (nodes MAX_NODES) (nodes 0))
(do ((i :: int 0 (+ i 1)))
    ((>= i MAX_NODES))
  ((nodes i):connect (nodes (+ i 1))))

((nodes 0):send-message (TokenMessage 1 0))
