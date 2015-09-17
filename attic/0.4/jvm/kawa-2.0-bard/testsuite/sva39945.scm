(let ((x::java.lang.Runnable
       (object (java.lang.Runnable)
               ((run) (format #t "foo~%")))))
  (x:run))
;; Output: foo
