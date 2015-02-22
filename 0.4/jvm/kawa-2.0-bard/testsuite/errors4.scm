;; test r7rs's syntax-error
(define-syntax simple-let
  (syntax-rules ()
    ((_ (head ... ((x . y) val) . tail)
       body1 body2 ...)
     (syntax-error "expected an identifier but got" (x . y)))
    ((_ ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))))
(list (simple-let (((a . b) 12)) (format #t "x:~w~%" x)))
(list (simple-let ((x 12)) (format #t "x:~w~%" x)))
;; Diagnostic: errors4.scm:10:7: expected an identifier but got (a . b)
