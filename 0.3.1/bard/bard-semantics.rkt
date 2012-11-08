(module bard-semantics racket
  (require data/gvector)
  (provide make-bard-class bard-class-members set-bard-class-members!
           make-bard-series bard-series-cache bard-series-ref)

  ;; actors
  
  ;; classes
  (struct bard-class ((members  #:mutable)))
  (define (make-bard-class)(bard-class '()))
  
  ;; functions
  ;; macros
  ;; methods
  ;; protocols
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
  
  ;; vectors
  
  )
