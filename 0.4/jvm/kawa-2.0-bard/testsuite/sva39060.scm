(module-export <PublicClass> <PublicInterface>)

(define-simple-class <PublicInterface> () interface: #t
  ((setup (argument :: <String>)) :: <void>
   #!abstract))

(define-simple-class <PublicClass> (<Object>))

(define-simple-class <PrivateClass> (<PublicInterface>)
  ((setup (argument :: <java.lang.String>)) :: <void>
   (format (current-output-port) "Argument was: ~a~%" argument)))

(format #t "Done.~%")
;; Output: Done.
