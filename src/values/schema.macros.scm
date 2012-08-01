
(define-macro (define-schema sname includes . slot-specs)
  `(begin
     (define ,sname (make-schema ,includes ,slotspecs))
     (table-set! $schemas ',sname ,sname)
     ,sname))

