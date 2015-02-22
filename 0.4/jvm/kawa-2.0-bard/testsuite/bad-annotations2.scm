(define-class MyClass ()
  (@java.lang.Override)
  (@MyAnnotType name: "MyClassName")
  interface: #f
  (x
   (@MyAnnotType
    name: (copy-string "myName")
    bvalue: 234569898989
    nosuchname: 123
    chvalue: #\B
    )
   ::integer)
  (y
   (@java.lang.Override)
   (@javax.annotation.Resource
    type: java.util.ArrayList)
   :: integer)
)
