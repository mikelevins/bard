(define-alias Resource javax.annotation.Resource)
(define-alias AuthenticationType javax.annotation.Resource:AuthenticationType)
(define-alias ElementType java.lang.annotation.ElementType)
;(define-constant CC javax.annotation.Resource:AuthenticationType:CONTAINER)
;(define-constant XX 20)

(define-class MyClass ()
  (@MyAnnotType name: "MyClassName")
  interface: #f
  (x #|(@java.lang.Deprecated)|#
   ;;     (@javax.jws.soap.InitParam name: (make-string 4)  value: 0)
   ;; Also array of annotation: SOAPMessageHandler
   ;(@javax.xml.bind.annotation.XmlSchemaTypes({ @javax.xml.bind.annotation.XmlSchemaType(...), @XmlSchemaType(...) })
   (|@MyAnnotType| ;; Test deprecated single-symbol form
    svalue: 4324
    name: "myName"
    ;names: #("name1" "name2")
    bvalue: 2
    ivalue: (+ 100 12)
    blvalue: (> 3 4)
    chvalue: #\B
    etype: ElementType:PACKAGE
    )
   ::integer)
  (y
   (@javax.annotation.Resource
    authenticationType: Resource:AuthenticationType:CONTAINER
    type: java.util.ArrayList)
   :: integer)
  ((toString)
   (@java.lang.Override)
   (@MyAnnotType
    names: (string[] "x" "y")
    clvalue: java.io.InputStream)
   (format "MyClass[x:~s y:~s]" x y))
)
#| 
     (@java.beans.ConstructorProperties value: #("abc" "def"))
     ::integer))
|#

;; A bonus test for using an alias to a sub-class.
(define (authentication-identity x::AuthenticationType) ::AuthenticationType
  x)
