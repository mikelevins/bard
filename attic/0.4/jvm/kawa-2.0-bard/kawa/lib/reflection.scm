(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define-syntax (primitive-constructor form)
  (syntax-case form ()
    ((_ class (type ...))
     (syntax-case (generate-temporaries (syntax (type ...))) ()
       ((name ...)
	(syntax (lambda (name ...) :: class
			(make class (as type name) ...))))))))

;;; RECORDS

(define (make-record-type (name :: <String>) (fnames :: <list>))
  (invoke-static <record> 'makeRecordType name fnames))

(define (record-constructor (cl :: <class-type>) #!optional (flds #!null))
  (make <kawa.lang.RecordConstructor> cl flds))

(define (record-accessor (class :: <class-type>) (fname :: <String>))
  (make <kawa.lang.GetFieldProc> class fname))

(define (record-modifier (class :: <class-type>) (fname :: <String>))
  (make <kawa.lang.SetFieldProc> class fname))

(define (record? obj)
  (instance? obj <record>))

(define (record-predicate rtype)
  (lambda (object)
    ((primitive-virtual-method <type> "isInstance" <boolean> (<object>))
     rtype object)))

(define (record-type-descriptor object)
  ((primitive-static-method <type> "make" <type> (<java.lang.Class>))
   ((primitive-virtual-method <object> "getClass"
			      <java.lang.Class> ())
    object)))

(define (record-type-name (rtd :: <class-type>))
  (invoke-static <gnu.expr.Compilation> 'demangleName
		  (invoke rtd 'getName) #t))

(define (record-type-field-names rtd)
  ((primitive-static-method <record> "typeFieldNames"
			    <list> (<class-type>))
   rtd))

#| THESE CAN BE FUNCTIONS WHEN WE HAVE BETTER INLINING:
(define (primitive-array-new element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayNew> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-set element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArraySet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-get element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayGet> (<gnu.bytecode.Type>))
   element-type))
(define (primitive-array-length element-type)
  ((primitive-constructor <gnu.kawa.reflect.ArrayLength> (<gnu.bytecode.Type>))
   element-type))
... etc ...
|#
(define-syntax primitive-array-new
  (syntax-rules ()
		((primitive-array-new element-type)
		 (constant-fold
		  make <gnu.kawa.reflect.ArrayNew> element-type))))

(define-syntax primitive-array-set
  (syntax-rules ()
		((primitive-array-set element-type)
		 (constant-fold
		  make <gnu.kawa.reflect.ArraySet> element-type))))

(define-syntax primitive-array-get
  (syntax-rules ()
		((primitive-array-get element-type)
		 (constant-fold
		  make <gnu.kawa.reflect.ArrayGet> element-type))))

(define-syntax primitive-array-length
  (syntax-rules ()
		((primitive-array-length element-type)
		 (constant-fold
		  make <gnu.kawa.reflect.ArrayLength> element-type))))
(define-syntax primitive-get-field
  (syntax-rules ()
		((primitive-get-field ctype fname ftype)
		 (constant-fold
		  make <kawa.lang.GetFieldProc>
		  ctype fname ftype 1 #|PUBLIC|#))))
(define-syntax primitive-set-field
  (syntax-rules ()
		((primitive-set-field ctype fname ftype)
		 (constant-fold
		  make <kawa.lang.SetFieldProc>
		  ctype fname ftype 1 #|PUBLIC|#))))

(define-syntax primitive-get-static
  (syntax-rules ()
		((primitive-get-static ctype fname ftype)
		 (constant-fold
		  make <gnu.kawa.reflect.StaticGet>
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))
(define-syntax primitive-set-static
  (syntax-rules ()
		((primitive-set-static ctype fname ftype)
		 (constant-fold
		  make <gnu.kawa.reflect.StaticSet>
		  ctype fname ftype 9 #|PUBLIC|STATIC|#))))

(define (subtype? (t1 <type>) (t2 <type>)) <boolean>
  ((primitive-virtual-method <type> "isSubtype" <boolean> (<type>))
   t1 t2))

#|
(define (field-location object (name <symbol>))
  ((primitive-static-method <kawa.lang.FieldLocation> "make"
                            <kawa.lang.FieldLocation> (<object> <symbol>))
   object name))

(define (method object (name <symbol>))
  ((primitive-static-method <kawa.lang.method> "make"
                            <kawa.lang.method> (<object> <symbol>))
   object name))
|#
