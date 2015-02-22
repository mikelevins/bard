(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.case_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.strings>)
(require <kawa.lib.ports>)

#|
(define (equal? x y) :: <boolean>
  (or (eq? x y)
      (and (not (eq? x #!null))
	   ((primitive-virtual-method <object> "equals" <boolean> (<object>))
	    x y))))
|#

(define (boolean? x) :: <boolean>
  (or (eq? x #t) (eq? x #f)))

(define (boolean=? b1 b2 #!rest (r ::object[])) ::boolean
  (let ((n (- r:length 1)))
    (if b1
        (and b2
             (let loop ((i ::int n))
               (or (< i 0) (and (r i) (loop (- i 1))))))
        (and (not b2)
             (let loop ((i ::int n))
               (or (< i 0) (and (not (r i)) (loop (- i 1)))))))))

(define (symbol? x) :: <boolean>
  (instance? x <gnu.mapping.Symbol>))

(define (symbol->string (s <symbol>)) :: constant-string
  (s:toString))

(define (symbol=? s1::symbol s2::symbol #!rest r)::boolean
  (and (gnu.mapping.Symbol:equals s1 s2)
       (or (null? r) (apply symbol=? s2 r))))

(define (symbol-local-name s::symbol) ::constant-string
  (s:getLocalPart))

(define (symbol-namespace s::symbol) ::namespace
  (s:getNamespace))

(define (symbol-namespace-uri s::symbol) ::constant-string
  (s:getNamespaceURI))

(define (symbol-prefix s::symbol) ::constant-string
  (s:getPrefix))

(define (namespace-uri (ns::gnu.mapping.Namespace)) ::string
  (invoke ns 'getName))

(define (namespace-prefix (ns::gnu.mapping.Namespace)) ::string
  (invoke ns 'getPrefix))

(define (string->symbol (str <string>))
  (gnu.mapping.SimpleSymbol:valueOf (str:toString)))

(define (procedure? x) :: <boolean>
  (or (instance? x <function>) (instance? x gnu.kawa.lispexpr.LangObjType)))

(define (values #!rest (args :: <Object[]>))
  (invoke-static <gnu.mapping.Values> 'make args))


(define (environment-bound? (env :: <gnu.mapping.Environment>) sym)
  :: <boolean>
  (invoke env 'isBound
	  (gnu.kawa.lispexpr.LispLanguage:langSymbolToSymbol sym)))

;; The version number is not optional according to R5RS.
;; But since earlier versions of this implementation took 0 arguments,
;; we'll make it optional for backwards compatibility, at least for now.
(define (null-environment #!optional version)
  (static-field <kawa.standard.Scheme> 'nullEnvironment))

(define (scheme-report-environment version)
  (case version
    ((4) (static-field <kawa.standard.Scheme> 'r4Environment))
    ((5) (static-field <kawa.standard.Scheme> 'r5Environment))
    (else (primitive-throw
           (kawa.lang.NamedException:makeError
            "scheme-report-environment version must be 4 or 5")))))

(define (interaction-environment)
  (invoke-static <gnu.mapping.Environment> 'user))

(define (scheme-implementation-version) :: constant-string
  (kawa.Version:getVersion))

(define (set-procedure-property! proc :: <procedure> key value)
  (invoke proc 'setProperty key value))

(define-procedure procedure-property
  setter: set-procedure-property!
  (begin
    (define (procedure-property (proc :: <procedure>) key #!optional default)
      (invoke proc 'getProperty key default))
    procedure-property))

(define (dynamic-wind before thunk after)
  (before)
  (try-finally
   (thunk)
   (after)))

(define (promise? obj) ::boolean
  (instance? obj gnu.mapping.Lazy))

(define (make-promise obj) ::gnu.mapping.Lazy
  (if (gnu.mapping.Lazy? obj) obj
      (gnu.mapping.Promise:makeBoundPromise obj)))

(define (promise-set-value! promise::gnu.mapping.Promise value) ::void
  (promise:setValue value))

(define (promise-set-alias! promise::gnu.mapping.Promise aliasee::gnu.mapping.Lazy) ::void
  (promise:setAlias aliasee))

(define (promise-set-exception! promise::gnu.mapping.Promise exception::java.lang.Throwable) ::void
  (promise:setException exception))

(define (promise-set-thunk! promise::gnu.mapping.Promise thunk::gnu.mapping.Procedure) ::void
  (promise:setThunk thunk))

(define (force arg)
  (gnu.mapping.Promise:force1 arg))

(define (force* arg)
  (gnu.mapping.Promise:force arg))

(define (eager value)
  eager)

(define (base-uri #!optional (node #!null))
  (let ((uri (if (eq? node #!null)
		 (gnu.kawa.io.Path:currentPath)
		 ((as <gnu.kawa.xml.KNode> node):baseURI))))
    (if (eq? uri #!void) #f uri)))

#|
(define (identity-function x)
  x)

(define (make-parameter init #!optional converter :: <procedure> identity-function)

  (make <gnu.kawa.util.Parameter> init converter))
|#

(define (gentemp) :: <symbol>
  (invoke-static <gnu.expr.Symbols> 'gentemp))

(define (add-procedure-properties
	 (proc :: <gnu.expr.GenericProc>)
	 #!rest (args :: <object[]>)) :: <void>
  (invoke proc 'setProperties args))

(define (features) ::list
  (kawa.standard.IfFeature:featureList))
