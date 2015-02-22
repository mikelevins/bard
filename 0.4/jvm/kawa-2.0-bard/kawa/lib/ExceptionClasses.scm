(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.misc>)

(define current-handler
  ::java.lang.ThreadLocal[HandlerLink]
  (java.lang.ThreadLocal))

;; Implements a stack of handler procedures.
(define-simple-class HandlerLink ()
  (handlerProc ::procedure)
  (outer ::HandlerLink)
  ((push handler::procedure)::HandlerLink allocation: 'static
   (let ((new (HandlerLink handlerProc: handler
                           outer: (current-handler:get))))
     (current-handler:set new)
     new))
  ((pop) ::void
   (current-handler:set outer))
  ((handle ex::java.lang.Throwable) ::java.lang.Throwable
   (current-handler:set outer)
   (if (not (kawa.lang.CalledContinuation? ex))
       (let ((cause (ExceptionWithValue:unwrap ex)))
         (handlerProc cause)))
   ex) ;;FIXME "secondary handler" ? Change
)

;; If raise/raise-continuable is called with a value that
;; is not a Thowable, wrap it in an ExceptionWithValue object.
(define-simple-class ExceptionWithValue (java.lang.RuntimeException)
  (payload)
  ((getMessage)::java.lang.String (payload:toString))
  ((wrap value) ::java.lang.Throwable allocation: 'static
   (if (java.lang.Throwable? value) value (ExceptionWithValue payload: value)))
  ((unwrap ex) allocation: 'static
   (if (ExceptionWithValue? ex) (->ExceptionWithValue ex):payload ex)))
