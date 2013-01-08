;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tags.scm
;;;; Project:       Bard
;;;; Purpose:       tools for working with built-in gambit type tags
;;;;                and bard-specific type tags
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; =====================================================================
;;; prelude
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; general utilities
;;; ---------------------------------------------------------------------

(define (%find-bignum)
  (let loop ((i 2))
    (if (##bignum? i)
        i
        (loop (* i i)))))

;;; ---------------------------------------------------------------------
;;; built-in schema types
;;; ---------------------------------------------------------------------

(define-type schema extender: define-schema name tag)
(define-type schema-instance extender: define-instance (schema instance-schema))

(define-schema primitive-schema)
(define-schema structure-schema prototype)
(define-schema base-schema)
(define-schema record-schema)
(define-instance record-instance slots)
(define-schema tuple-schema)
(define-instance tuple-instance elements)
(define-schema union-schema)
(define-instance union-instance variants)

(define-schema foreign-schema type-name)

;;; ---------------------------------------------------------------------
;;; gambit type tags
;;; ---------------------------------------------------------------------

(define tags:$gambit-fixnum 0)
(define tags:$gambit-subtyped 1)
(define tags:$gambit-special 2)
(define tags:$gambit-pair 3) ; BUG: not necessarily correct on all architectures

(define tags:$gambit-subtype-structure 4)

(define (%gambit-type object)(##type object))
(define (%gambit-special? object)(= 2 (##type object)))

(define (%gambit-subtype object)
  (if (##subtyped? object)
      (##subtype object)
      0))

;;; ---------------------------------------------------------------------
;;; gambit "special" objects
;;; ---------------------------------------------------------------------
;;; "special" objects have gambit tag 2
;;; they are immediates representing values like '(), #!eof, and characters
;;; we give them bard tags to distinguish them because gambit doesn't
;;; use tags for that purpose, and we want a uniform type id

(define $max-bard-special-number 255)
(define $unercognized-bard-special-number $max-bard-special-number)

(define (%bard-special-number object)
  (cond
   ((null? object) 0)
   ((boolean? object) 1)
   ((char? object) 2)
   ((eqv? #!void object) 3)
   ((eqv? #!unbound object) 4)
   ((eqv? #!eof object) 5)
   (else $max-bard-special-number)))

;;; =====================================================================
;;; obtaining tags
;;; =====================================================================

;;; a type tag is a 30-bit integer used to store the type and subtype
;;; bits of a gambit value uniformly for indexing purposes. In the
;;; bard integer, the two least-significant bits are the gambit type;
;;; the rest is the gambit subtype.

(define $type-mask      #b000000000000000000000000000011)
(define $subtype-mask   #b000000000000000000001111111100)

(define (integer->subtype n)(arithmetic-shift n 2))
(define (subtype->integer n)(arithmetic-shift n -2))

(define (tag->gambit-type n)(bitwise-and n $type-mask))
(define (tag->gambit-subtype n)(subtype->integer (bitwise-and n $subtype-mask)))

;;; bard type numbers
;;; ---------------------------------------------------------------------
;;; a bard type number is a number assigned to a schema to distinguish
;;; it from other. since gambit type and subtype together occupy only
;;; 7 bits, the tag scheme permits us to store bard type numbers in
;;; tags.  reserve bits 0-9 for gambit type and subtype, and bits
;;; 10-29 for bard type numbers.

(define $next-bard-type-number (+ 1 $max-bard-special-number))
(define (%next-bard-type-number)
  (let ((num $next-bard-type-number))
    (set! $next-bard-type-number (+ 1 $next-bard-type-number))
    num))

(define $bard-type-mask #b111111111111111111110000000000)

(define (integer->bard-type n)(arithmetic-shift n 10))
(define (bard-type->integer n)(arithmetic-shift n -10))

(define (%make-tag type-tag subtype-tag #!optional (bard-tag 0))
  (+ type-tag
     (integer->subtype subtype-tag)
     (integer->bard-type bard-tag)))

(define (tag->bard-type n)(bard-type->integer (bitwise-and n $bard-type-mask)))

(define (%tag val)
  (if (schema-instance? val)
      (schema-tag (instance-schema val))
      (if (##structure? val)
          (let* ((struct (##structure-type val))
                 (schema (%structure->schema struct)))
            (if schema
                (schema-tag schema)
                #f))
          (if (%gambit-special? val)
              (%make-tag (%gambit-type val) 0 (%bard-special-number val))
              (%make-tag (%gambit-type val) (%gambit-subtype val))))))

;;; ---------------------------------------------------------------------
;;; well-known gambit types
;;; ---------------------------------------------------------------------

(define tags:$undefined (%tag #!unbound))
(define tags:$null (%tag '()))
(define tags:$boolean (%tag #t))
(define tags:$character (%tag #\c))
(define tags:$fixnum (%tag 1))
(define tags:$bignum (%tag (%find-bignum)))
(define tags:$flonum (%tag 1.2))
(define tags:$ratnum (%tag 2/3))
(define tags:$string (%tag "foo"))
(define tags:$pair (%tag '(a . b)))
(define tags:$symbol (%tag 'foo))
(define tags:$keyword (%tag foo:))
(define tags:$procedure (%tag (lambda (x) x)))
(define tags:$vector (%tag (vector)))
(define tags:$box (%tag (box 1)))


;;; =====================================================================
;;; the schema registry
;;; =====================================================================
;;; the data structures and functions here enable us to recover a schema
;;; given one of its instances.

;;; handling primitive schemas
;;; ---------------------------------------------------------------------

(define +tag->schema-registry+ (make-table test: eqv?))

;;; we recover the tag from the value itself, using gambit's
;;; tag operations (##type and ##subtype), then look up the
;;; schema in the registry.

(define (%register-primitive-schema! sc tag)
  (table-set! +tag->schema-registry+ tag sc))

(define (%tag->schema tag)
  (table-ref +tag->schema-registry+ tag #f))

;;; handling structure schemas
;;; ---------------------------------------------------------------------
;;; recover the structure used to create the instance using
;;; ##structure-type, then recover the schema from the structure
;;; registry

(define +structure->schema-registry+ (make-table test: eqv?))

(define (%register-structure-schema! structure-type schema)
  (table-set! +structure->schema-registry+ structure-type schema))

(define (%structure->schema struct)
  (table-ref +structure->schema-registry+ struct #f))

;;; handling foreign schemas
;;; ---------------------------------------------------------------------
;;; registry for foreign schemas
(define +foreign-name->schema-registry+ (make-table test: eqv?))

;;; base schemas
;;; ---------------------------------------------------------------------
;;; we recover the schema from the instance, using instance-schema; no
;;; registry is necessary

;;; =====================================================================
;;; schema definitions
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; primitive schemas
;;; ---------------------------------------------------------------------

(define <undefined> (make-primitive-schema '<undefined> tags:$undefined))
(%register-primitive-schema! <undefined> tags:$undefined)
(define <null> (make-primitive-schema '<null> tags:$null))
(%register-primitive-schema! <null> tags:$null)
(define <character> (make-primitive-schema '<character> tags:$character))
(%register-primitive-schema! <character> tags:$character)
(define <boolean> (make-primitive-schema '<boolean> tags:$boolean))
(%register-primitive-schema! <boolean> tags:$boolean)
(define <symbol> (make-primitive-schema '<symbol> tags:$symbol))
(%register-primitive-schema! <symbol> tags:$symbol)
(define <keyword> (make-primitive-schema '<keyword> tags:$keyword))
(%register-primitive-schema! <keyword> tags:$keyword)
(define <flonum> (make-primitive-schema '<flonum> tags:$flonum))
(%register-primitive-schema! <flonum> tags:$flonum)
(define <ratnum> (make-primitive-schema '<ratnum> tags:$ratnum))
(%register-primitive-schema! <ratnum> tags:$ratnum)
(define <fixnum> (make-primitive-schema '<fixnum> tags:$fixnum))
(%register-primitive-schema! <fixnum> tags:$fixnum)
(define <bignum> (make-primitive-schema '<bignum> tags:$bignum))
(%register-primitive-schema! <bignum> tags:$bignum)
(define <primitive-procedure> (make-primitive-schema '<primitive-procedure> tags:$procedure))
(%register-primitive-schema! <primitive-procedure> tags:$procedure)
(define <string> (make-primitive-schema '<string> tags:$string))
(%register-primitive-schema! <string> tags:$string)
(define <pair> (make-primitive-schema '<pair> tags:$pair))
(%register-primitive-schema! <pair> tags:$pair)

(define (%undefined? x)(eqv? x #!unbound))
(define (%defined? x)(not (%undefined? x)))

;;; =====================================================================
;;; base schemas
;;; =====================================================================

;;; ----------------------------------------------------------------------
;;; classes
;;; ----------------------------------------------------------------------

(define tags:$bard-class (%next-bard-type-number))
(define <class> (make-base-schema '<class> tags:$bard-class))

(define-instance class-instance constructor: make-class-instance (name class-name))

;;; constructor

(define (%make-class name)(make-class-instance <class> name))

;;; definitions of classes
;;; ----------------------------------------------------------------------
;;; convention: class names are nouns

(define & (%make-class '&)) ; the class of optional arguments
(define Anything (%make-class 'Anything))
(define Applicable (%make-class 'Applicable))
(define Boolean (%make-class 'Boolean))
(define Character (%make-class 'Character))
(define Class (%make-class 'Class))
(define Float (%make-class 'Float))
(define Fraction (%make-class 'Fraction))
(define Function (%make-class 'Function))
(define InputStream (%make-class 'InputStream))
(define Integer (%make-class 'Integer))
(define Keyword (%make-class 'Keyword))
(define List (%make-class 'List))
(define Method (%make-class 'Method))
(define Null (%make-class 'Null))
(define Number (%make-class 'Number))
(define Orderable (%make-class 'Orderable))
(define OutputStream (%make-class 'OutputStream))
(define Pair (%make-class 'Pair))
(define Protocol (%make-class 'Protocol))
(define Ratio (%make-class 'Ratio))
(define Schema (%make-class 'Schema))
(define Stream (%make-class 'Stream))
(define Symbol (%make-class 'Symbol))
(define Table (%make-class 'Table))
(define Text (%make-class 'Text))
(define Type (%make-class 'Type))
(define Undefined (%make-class 'Undefined))

;;; ----------------------------------------------------------------------
;;; <protocol>
;;; ----------------------------------------------------------------------

(define tags:$bard-protocol (%next-bard-type-number))
(define <protocol> (make-base-schema '<protocol> tags:$bard-protocol))

(define-instance protocol-instance 
  constructor: make-protocol-instance
  (name protocol-name)
  (functions protocol-functions))

;;; constructor

(define (%make-protocol name)(make-protocol-instance <protocol> name (make-table test: eq?)))

;;; accessors

(define (%protocol-ref p fn-name)
  (table-ref (protocol-functions p) fn-name #f))

(define (%protocol-add! p fn-name fn)
  (table-set! (protocol-functions p) fn-name fn)
  p)

;;; definitions of protocols
;;; ----------------------------------------------------------------------
;;; convention: protocol names are present participles

(define Applying       (%make-protocol 'Applying))       ; applying function-like values
(define Calculating    (%make-protocol 'Calculating))    ; performing arithmetic and other calculating tasks
(define Comparing      (%make-protocol 'Comparing))      ; comparing values for equality
(define Constructing   (%make-protocol 'Constructing))   ; constructing values
(define Converting     (%make-protocol 'Converting))     ; producing values of one type based on inputs of another
(define Listing        (%make-protocol 'Listing))        ; arranging values in lists
(define Mapping        (%make-protocol 'Mapping))        ; arranging values in tables
(define Ordering       (%make-protocol 'Ordering))       ; arranging values by magnitude
(define Pairing        (%make-protocol 'Pairing))        ; arranging values in pairs
(define Reading        (%make-protocol 'Reading))        ; getting values from input streams
(define System         (%make-protocol 'System))         ; system tools
(define TextProcessing (%make-protocol 'TextProcessing)) ; processing text
(define Typing         (%make-protocol 'Typing))         ; discriminating values by type
(define Writing        (%make-protocol 'Writing))        ; putting values into output streams

;;; ----------------------------------------------------------------------
;;; <alist-table>
;;; ----------------------------------------------------------------------

(define tags:$bard-alist-table (%next-bard-type-number))
(define <alist-table> (make-base-schema '<alist-table> tags:$bard-alist-table))

(define-instance alist-table-instance constructor: make-alist-table-instance slots)

;;; constructor

(define (%make-alist-table slots-alist)
  (let ((slots (map (lambda (s)(cons (car s)(cdr s)))
                    slots-alist)))
    (make-alist-table-instance <alist-table> slots)))

;;; accessors

(define alist-table-slots alist-table-instance-slots)
(define set-alist-table-slots! alist-table-instance-slots-set!)

(define (alist-table-get table key)
  (let* ((slots (alist-table-instance-slots table))
         (slot (assoc key slots)))
    (if slot
        (cdr slot)
        '())))

(define (alist-table-keys table)
  (let loop ((slots (alist-table-instance-slots table))
             (keys '()))
    (if (null? slots)
        (reverse keys)
        (let ((slot (car slots))
              (more (cdr slots)))
          (if (member (car slot) keys)
              (loop more keys)
              (loop more (cons (car slot) keys)))))))

(define (alist-table-put table key val)
  (make-alist-table-instance <alist-table> 
                             (cons (cons key val)
                                   (alist-table-instance-slots table))))

(define (alist-table-vals table)
  (let loop ((slots (alist-table-instance-slots table))
             (keys '())
             (vals '()))
    (if (null? slots)
        (reverse vals)
        (let ((slot (car slots))
              (more (cdr slots)))
          (if (member (car slot) keys)
              (loop more keys vals)
              (loop more
                    (cons (car slot) keys)
                    (cons (cdr slot) vals)))))))


;;; ----------------------------------------------------------------------
;;; <function>
;;; ----------------------------------------------------------------------

(define tags:$bard-function (%next-bard-type-number))
(define <function> (make-base-schema '<function> tags:$bard-function))

(define-instance function-instance
  constructor: make-function-instance
  name proc input-classes output-classes thunk-method method-tree)

;;; constructor

(define (%add-method! fn argtypes method)
  (let ((method-tree (function-method-tree fn)))
    (if (null? argtypes)
        (set-function-thunk-method! fn method)
        (apply %singleton-tree-put! method method-tree argtypes))
    fn))

(define (%add-primitive-method! fn argtypes method-proc #!key (debug-name #f)(restarg #f))
  (let* ((required-count (length argtypes))
         (method (make-primitive debug-name: debug-name
                                 procedure: method-proc
                                 required-count: required-count
                                 restarg: restarg)))
    (%add-method! fn argtypes method)
    fn))

(define (%search-method-tree-for-value mtree val)
  (let* ((sing (%existing-singleton val))
         (found (if sing (%singleton-tree-ref mtree sing) #f)))
    (or found
        (let* ((tp (%object->schema val))
               (found (%singleton-tree-ref mtree tp)))
          (or found
              (%singleton-tree-ref mtree Anything))))))

(define (%search-method-tree-for-values mtree vals)
  (if (null? vals)
      #f
      (let ((found (%search-method-tree-for-value mtree (car vals))))
        (if found
            (if (null? (cdr vals))
                (if (%singleton-tree? found)
                    #f
                    found)
                (if (%singleton-tree? found)
                    (%search-method-tree-for-values found (cdr vals))
                    #f))
            #f))))

(define (%function-best-method fn vals)
  (if (null? vals)
      (function-thunk-method fn)
      (%search-method-tree-for-values (function-method-tree fn) vals)))

(define (make-function #!key 
                       (debug-name 'an-anonymous-function)
                       (input-classes '())
                       (output-classes `(,Anything)))
  (let* ((fn (make-function-instance <function> debug-name #f input-classes output-classes #f (%singleton-tree)))
         (fn-proc (lambda args
                    (let ((best-method (%function-best-method fn args)))
                      (if best-method
                          (%apply best-method args)
                          (error (str "No applicable method for " fn " with arguments " args)))))))
    (set-function-proc! fn fn-proc)
    fn))

;;; accessors

(define function? function-instance?)
(define function-name function-instance-name)
(define function-input-classes function-instance-input-classes)
(define function-output-classes function-instance-output-classes)
(define function-proc function-instance-proc)
(define set-function-proc! function-instance-proc-set!)
(define function-thunk-method function-instance-thunk-method)
(define set-function-thunk-method! function-instance-thunk-method-set!)
(define function-method-tree function-instance-method-tree)

;;; ----------------------------------------------------------------------
;;; <interpreted-method>
;;; ----------------------------------------------------------------------

(define tags:$bard-interpreted-method (%next-bard-type-number))
(define <interpreted-method> (make-base-schema '<interpreted-method> tags:$bard-interpreted-method))

(define-instance interpreted-method-instance
  constructor: make-interpreted-method-instance
  name proc formals restarg required-count environment body)

;;; constructor

(define (%method-lexical-environment env params rest vals)
  (let loop ((env env)
             (formals params)
             (args vals))
    (if (null? args)
        ;; out of args
        (if (null? formals)
            (if rest (%add-binding env rest args) env)
            (error (str "Not enough arguments: " vals)))
        ;; more args to process
        (if (null? formals)
            (if rest
                (%add-binding env rest args)
                (error (str "Too many arguments: " vals)))
            (loop (%add-binding env (car formals)(car args))
                  (cdr formals)
                  (cdr args))))))

(define (make-interpreted-method #!key
                                 (formal-parameters '())
                                 (restarg #f)
                                 (body '(begin))
                                 (debug-name 'an-anonymous-interpreted-method)
                                 (environment (%null-environment)))
  (let* ((required-count (length formal-parameters))
         (method (make-interpreted-method-instance 
                  <interpreted-method> debug-name #f formal-parameters restarg required-count environment body))
         (method-proc (lambda args
                        (let* ((argcount (length args)))
                          (if (< argcount required-count)
                              (error (str "Expected " required-count "arguments, but found " (length args)))
                              (let* ((env (%method-lexical-environment (interpreted-method-environment method)
                                                                       formal-parameters restarg args)))
                                (%eval body env)))))))
    (set-interpreted-method-proc! method method-proc)
    method))

;;; accessors

(define interpreted-method? interpreted-method-instance?)
(define interpreted-method-name interpreted-method-instance-name)
(define interpreted-method-proc interpreted-method-instance-proc)
(define set-interpreted-method-proc! interpreted-method-instance-proc-set!)
(define interpreted-method-formals interpreted-method-instance-formals)
(define interpreted-method-restarg interpreted-method-instance-restarg)
(define interpreted-method-required-count interpreted-method-instance-required-count)
(define interpreted-method-environment interpreted-method-instance-environment)
(define set-interpreted-method-environment! interpreted-method-instance-environment-set!)
(define interpreted-method-body interpreted-method-instance-body)

;;; ----------------------------------------------------------------------
;;; <primitive>
;;; ----------------------------------------------------------------------

(define tags:$bard-primitive (%next-bard-type-number))
(define <primitive> (make-base-schema '<primitive> tags:$bard-primitive))

(define-instance primitive-instance
  constructor: make-primitive-instance
  name proc required-count restarg)

;;; constructor

(define (make-primitive #!key
                        (procedure #f)
                        (required-count 0)
                        (restarg #f)
                        (debug-name 'an-anonymous-primitive))
  (let* ((prim (make-primitive-instance <primitive> debug-name #f required-count restarg))
         (prim-proc (lambda args (apply procedure args))))
    (set-primitive-proc! prim prim-proc)
    prim))

;;; accessors

(define primitive-name primitive-instance-name)
(define primitive-proc primitive-instance-proc)
(define set-primitive-proc! primitive-instance-proc-set!)
(define primitive-restarg primitive-instance-restarg)
(define primitive-required-count primitive-instance-required-count)

;;; ----------------------------------------------------------------------
;;; <singleton>
;;; ----------------------------------------------------------------------

(define tags:$bard-singleton (%next-bard-type-number))
(define <singleton> (make-base-schema '<singleton> tags:$bard-singleton))

(define-instance singleton-instance constructor: make-singleton-instance value)

;;; singleton registry

(define +singletons+ (make-table test: equal?))

(define (%existing-singleton val)
  (table-ref +singletons+ val #f))

(define (%register-singleton! val sing)
  (table-set! +singletons+ val sing))

;;; constructor

(define (%singleton val)
  (or (%existing-singleton val)
      (let ((sing (make-singleton-instance <singleton> val)))
        (%register-singleton! val sing)
        sing)))

;;; accessors

(define singleton-value singleton-instance-value)
(define %singleton? singleton-instance?)

;;; ----------------------------------------------------------------------
;;; <generator>
;;; ----------------------------------------------------------------------

(define tags:$bard-generator (%next-bard-type-number))
(define <generator> (make-base-schema '<generator> tags:$bard-generator))

(define-instance generator-instance 
  constructor: make-generator-instance
  results)

;;; accessors

(define generator-results generator-instance-results)
(define set-generator-results! generator-instance-results-set!)

;;; constructor

(define (make-generator vars initvals body env)
  (let* ((gen (make-generator-instance <generator> '())))
    (let* ((returnc #f)
           (resumec #f)
           (loopc #f)
           (args initvals)
           (results '())
           (yield (make-primitive
                   procedure: (lambda args 
                                (call/cc
                                 (lambda (resume)
                                   (begin
                                     (set! resumec resume)
                                     (apply returnc args)))))
                   debug-name: 'yield
                   required-count: 0
                   restarg: 'more))
           (resume (make-primitive
                    procedure: (lambda vals (begin (set! args vals)(loopc)))
                    debug-name: 'resume
                    required-count: 0
                    restarg: 'more))
           (env (%add-let-bindings env
                                   `((yield ,yield)
                                     (resume ,resume))))
           (method (%eval `(method ,vars ,@body) env)))
      (lambda ()
        (call/cc 
         (lambda (return)
           (set! returnc return)
           (if resumec
               (resumec)
               (let loop ()
                 (set! loopc loop)
                 (%apply method args)
                 (loop)))))))
    gen))

;;; (define $g (make-generator '(x y) '(1 1) '((yield x)(resume y (+ x y))) '()))
;;; (next $g)

(define (next g)(g))

;;; =====================================================================
;;; structure schemas
;;; =====================================================================

(define <iostream> (make-structure-schema '<iostream> (%next-bard-type-number) (##structure-type (current-input-port))))
(%register-structure-schema! (structure-schema-prototype <iostream>) <iostream>)

;;; =====================================================================
;;; getting bard types for values
;;; =====================================================================

(define (%object->schema val)
  (if (schema-instance? val)
      (instance-schema val)
      (if (##structure? val)
          (%structure->schema (##structure-type val))
          (%tag->schema (%tag val)))))


