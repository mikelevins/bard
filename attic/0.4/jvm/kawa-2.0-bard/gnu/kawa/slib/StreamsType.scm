;; Internal and type definitions for Streams

(define-simple-class Stream (gnu.mapping.Lazy) interface: #t)

(define-simple-class StreamPromise (gnu.mapping.Promise Stream)
  ((*init* thunk::procedure lazy::boolean)
   (invoke-special gnu.mapping.Promise(this) '*init* thunk)
   ((this):setForceValueIfPromise lazy))
  ((*init*)
   (invoke-special gnu.mapping.Promise(this) '*init*)))

(define-simple-class StreamPair (gnu.lists.ImmutablePair Stream)
  ((*init* x y) (invoke-special gnu.lists.ImmutablePair (this) '*init* x y))
  ((getValue) (this)))

(define-alias stream-type Stream)

(define stream-null-1
  (let ((p (StreamPromise)))
    (invoke p' setValue '())
    p))

(define-syntax stream-lazy
  (syntax-rules ()
    ((_ expr)
     (StreamPromise (lambda () expr) #t))))

(define-syntax stream-delay
  (syntax-rules ()
    ((_ expr)
     (StreamPromise  (lambda () expr) #f))))

(define (stream-force promise)
  (force promise))

(define (stream-eager expr)
  expr)
