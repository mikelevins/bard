(module bard-semantics racket
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
    (bard-series '() generator))
  
  (define (bard-series-ref ser i)
    (let loop ((k (length (bard-series-cache ser))))
      (if (> k i)
          (list-ref (bard-series-cache ser) 
                    (- (- k 1) i))
          (let* ((gen (bard-series-generator ser))
                 (next (gen)))
            (set-bard-series-cache! ser (cons next (bard-series-cache ser)))
            (loop (length (bard-series-cache ser)))))))
  
  ;; vectors
  
  )
