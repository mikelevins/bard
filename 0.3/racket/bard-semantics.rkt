(module bard-semantics racket
  (require data/gvector)
  (provide make-bard-class bard-class-members set-bard-class-members!
           make-bard-function bard-function-parameter-classes bard-function-return-classes
           make-bard-protocol
           make-bard-series bard-series-cache bard-series-ref)
  
  ;; actors
  
  ;; classes
  (struct bard-class ((members  #:mutable)))
  (define (make-bard-class)(bard-class '()))
  
  ;; functions
  (struct bard-function (parameter-classes return-classes method-table))
  
  (define (make-bard-function parameter-classes return-classes)
    (bard-function parameter-classes return-classes (make-hash)))
  
  (define (bard-assert-class! cls tp)
    (if (not (member tp (bard-class-members cls)))
        (begin
          (set-bard-class-members! cls (cons tp (bard-class-members cls)))
          cls)
        cls))
  
  (define (bard-assert-classes! classes concrete-types)
    (map bard-assert-class! classes concrete-types))
  
  (define (bard-add-method! fn argtypes mlambda)
    (hash-set! (bard-function-method-table fn) 
               argtypes mlambda)
    (bard-assert-classes! (bard-function-parameter-classes fn) argtypes)
    fn)
    
  ;; protocols
  (struct bard-protocol (members))
  (define (make-bard-protocol name/function-alist)
    (bard-protocol (make-hasheq name/function-alist)))
  
  ;; records
  
  ;; series
  (struct bard-series 
    ((cache #:mutable)
     (generator)))
  
  (define (make-bard-series generator)
    (bard-series (make-gvector) generator))
  
  (define (bard-series-ref ser i)
    (let loop ((k (gvector-count (bard-series-cache ser))))
      (if (> k i)
          (gvector-ref (bard-series-cache ser) i)
          (let* ((gen (bard-series-generator ser))
                 (next (gen)))
            (gvector-add! (bard-series-cache ser) next)
            (loop (gvector-count (bard-series-cache ser)))))))
  
  )


