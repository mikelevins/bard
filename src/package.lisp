;;;; package.lisp

(EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (SETF (READTABLE-CASE *READTABLE*) :PRESERVE))

(DEFPACKAGE :bard.internal
  (:USE :CL)
  (:EXPORT
   #:^
   #:begin
   #:bind
   #:call
   #:define
   #:false
   #:if
   #:set!
   #:true
   #:unless
   #:when))

(DEFPACKAGE :bard
  (:USE :CL :bard.internal))


