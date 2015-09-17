;"pp.scm" Pretty-print

(require 'generic-write)

; (pretty-print obj port) pretty prints 'obj' on 'port'.  The current
; output port is used if 'port' is not specified.

(define (pretty-print obj #!optional (port (current-output-port)))
  (generic-write obj #f 79 (lambda (s) (display s port) #t)))
