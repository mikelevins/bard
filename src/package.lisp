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
   #:set!))

(DEFPACKAGE :bard
  (:USE :CL :bard.internal))
