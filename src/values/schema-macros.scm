(define (%parse-slots-for-%define slot-specs)
  `(list ,@(map (lambda (s)
                  (cond
                   ((symbol? s) `(list (quote ,s)))
                   ((list? s) `(list (quote ,(car s))
                                     default:
                                     ,(getf default: s)))
                   (else (error (string-append "Malformed slot-spec"
                                               (object->string s)))))) 
                slot-specs)))

(define-macro (define-schema sname includes . slot-specs)
  `(begin
      (define ,sname (%make-schema ',sname (list ,@includes) ,(%parse-slots-for-%define slot-specs)))
      ,sname))

