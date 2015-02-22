(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define (sleep (time :: <quantity>)) :: <void>
  (kawa.standard.sleep:sleep time))

;; FUTURE

(define-syntax future (syntax-rules ()
				   ((future expression)
				    (%make-future (lambda () expression)))))

(define-private (%make-future (p :: <procedure>)) :: <gnu.mapping.Future>
  (let ((f :: <gnu.mapping.Future> (make <gnu.mapping.Future> p)))
    (invoke f 'start)
    f))

(define (runnable (p :: <procedure>)) :: <gnu.mapping.RunnableClosure>
  (make <gnu.mapping.RunnableClosure> p))
