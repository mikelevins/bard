;;;; package.lisp

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
   #:not
   #:or
   #:set!
   #:true
   #:unless
   #:when))

(DEFPACKAGE :bard
  (:USE :CL :bard.internal))


