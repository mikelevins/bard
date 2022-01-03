;;;; package.lisp

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))

(DEFPACKAGE :bard.internal
  (:USE :CL)
  (:EXPORT
   #:^
   #:and
   #:apply
   #:begin
   #:bind
   #:call
   #:define
   #:false
   #:function
   #:if
   #:method
   #:not
   #:or
   #:set!
   #:true
   #:unless
   #:when))

(DEFPACKAGE :bard
  (:USE :CL :bard.internal))


