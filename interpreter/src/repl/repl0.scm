;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%initial-bard-environment)
  (%extend-environment 
   (%null-environment)
   (list
    ;; general
    'id (lambda (x) x)
    ;; numbers
    '+ prim:+
    '- prim:-
    '* prim:*
    '/ prim:/
    'odd? odd?
    'even? even?
    'random (lambda (n)(random-integer n))
    ;; Character
    'character? char?
    ;; List
    'list prim:list
    ;; Frame
    'frame prim:frame
    ;; representations
    '<undefined> <undefined>
    '<null> <null>
    '<character> <character>
    '<boolean> <boolean>
    '<symbol> <symbol>
    '<keyword> <keyword>
    '<flonum> <flonum>
    '<ratnum> <ratnum>
    '<fixnum> <fixnum>
    '<bignum> <bignum>
    '<primitive-procedure> <primitive-procedure>
    '<cons> <cons>
    '<string> <string>
    '<input-stream> <input-stream>
    '<output-stream> <output-stream>
    '<frame> <frame>
    '<function> <function>
    '<method> <method>

    ;; protocols
    'Anything Anything
    'something? bard:something?

    'Applicable Applicable
    'applicable? bard:applicable?
    'apply bard:apply

    'As As
    'as bard:as

    'Atom Atom
    'atom? bard:atom?

    'Boolean Boolean
    'boolean? bard:boolean?
    'false? bard:false?
    'true? bard:true?

    'Character Character
    'character? bard:character?

    'Comparable Comparable
    '= bard:=
    '> bard:>
    '< bard:<
    '>= bard:>=
    '<= bard:<=

    'Float Float
    'float? bard:float?

    'ForeignValue ForeignValue
    'foreign-value? bard:foreign-value?
    
    'Frame Frame
    'frame? bard:frame?
    'contains-key? bard:contains-key?
    'contains-value? bard:contains-value?
    'get bard:get
    'keys bard:keys
    'merge bard:merge
    'put bard:put
    'vals bard:vals

    'Function Function
    'function? bard:function?

    'Integer Integer
    'integer? bard:integer?

    'IOStream IOStream
    'close bard:close
    'current-input bard:current-input
    'current-output bard:current-output
    'display bard:display
    'input-stream? bard:input-stream?
    'iostream? bard:iostream?
    'load bard:load
    'open bard:open
    'output-stream? bard:output-stream?
    'print bard:print
    'read bard:read
    'read-file bard:read-file
    'read-line bard:read-line
    'read-lines bard:read-lines
    'show bard:show
    'write bard:write

    'Keyword Keyword
    'keyword? bard:keyword?

    'List List
    'add-first bard:add-first
    'add-last bard:add-last
    'any bard:any
    'append bard:append
    'contains? bard:contains?
    'difference bard:difference
    'drop bard:drop
    'drop-before bard:drop-before
    'element bard:element
    'empty? bard:empty?
    'every? bard:every?
    'filter bard:filter
    'find bard:find
    'first bard:first
    'interleave bard:interleave
    'interpose bard:interpose
    'intersection bard:intersection
    'last bard:last
    'length bard:length
    'list? bard:list?
    'map bard:map
    'merge bard:merge
    'partition bard:partition
    'position bard:position
    'range bard:range
    'reduce bard:reduce
    'repeat bard:repeat
    'reverse bard:reverse
    'second bard:second
    'select bard:select
    'shuffle bard:shuffle
    'slice bard:slice
    'some? bard:some?
    'sort bard:sort
    'tail bard:tail
    'tails bard:tails
    'take bard:take
    'take-before bard:take-before
    'unique bard:unique
    'unzip bard:unzip
    'zip bard:zip

    'Method Method
    'method? bard:method?

    'Name Name
    'name? bard:name?

    'Null Null
    'nothing? bard:nothing?

    'Number Number
    'number? bard:number?

    'PrimitiveValue PrimitiveValue
    'primitive-value? bard:primitive-value?

    'Procedure Procedure
    'procedure? bard:procedure?

    'Ratio Ratio
    'ratio? bard:ratio?

    'StructureValue StructureValue
    'structure-value? bard:structure-value?

    'Symbol Symbol
    'symbol? bard:symbol?

    'Text Text
    'text? bard:text?

    'Type Type
    'type bard:type
    'type? bard:type?

    'Undefined Undefined
    'undefined? bard:undefined?

    )))


(define *bard-banner* "Bard 0.1")
(define *bard-prompt* "bard> ")

(define (display-error err)
  (let ((msg (cond
              ((error-exception? err) (error-exception-message err))
              ((heap-overflow-exception? err) "Heap overflow")
              ((stack-overflow-exception? err) "Stack overflow")
              ((os-exception? err) (string-append "OS exception: " (object->string (os-exception-code err))))
              ((unbound-os-environment-variable-exception? err)
               (string-append "Unbound OS environment variable; "
                              "procedure: " (object->string (unbound-os-environment-variable-exception-procedure err))
                              " arguments: " (object->string (unbound-os-environment-variable-exception-arguments err))))
              ((no-such-file-or-directory-exception? err) )
              ((scheduler-exception? err) "Scheduler exception")
              ((deadlock-exception? err) "Deadlock exception")
              ((abandoned-mutex-exception? err) "Abandoned mutex")
              ((join-timeout-exception? err) "Join timeout")
              ((started-thread-exception? err) "Thread already started")
              ((terminated-thread-exception? err) "Thread has already terminated")
              ((uncaught-exception? err) "Uncaught exception")
              ((cfun-conversion-exception? err) "error converting a foreign value")
              ((sfun-conversion-exception? err) "error converting to a foreign value")
              ((multiple-c-return-exception? err) "error returning from a foreign procedure")
              ((datum-parsing-exception? err) 
               (string-append "error parsing a datum: " (object->string (datum-parsing-exception-kind err)) " "
                              (object->string (datum-parsing-exception-parameters err))))
              ((expression-parsing-exception? err) 
               (string-append "parse error parsing an expression: " (object->string (expression-parsing-exception-kind err)) " "
                              (object->string (expression-parsing-exception-parameters err))))
              ((unbound-global-exception? err) (string-append "unbound global variable: "
                                                              (object->string (unbound-global-exception-variable err))))
              ((type-exception? err)
               (string-append "type error parsing an expression: " (object->string (type-exception-procedure err)) " "
                              (object->string (type-exception-arguments err))))
              ((range-exception? err) "argument out of range")
              ((divide-by-zero-exception? err) "divide by zero")
              ((improper-length-list-exception? err) "list is the wrong length")
              ((wrong-number-of-arguments-exception? err) "wrong number of arguments")
              ((number-of-arguments-limit-exception? err) "too many arguments")
              ((nonprocedure-operator-exception? err) "operator is not a procedure")
              ((unknown-keyword-argument-exception? err) "unknown keyword argument")
              ((keyword-expected-exception? err) "keyword argument expected")
              ((noncontinuable-exception? err) "noncontinuable exception")
              (else "unrecognized error"))))
    (display (string-append (string #\newline) "ERROR: " msg " " (string #\newline)))))

(define (bard:repl)
  (set! $bard-toplevel-environment (%initial-bard-environment))
  (newline)
  (display *bard-banner*)
  (newline)
  (read-line)
  (let loop ()
    (newline)
    (display *bard-prompt*)
    (let ((error-handler (lambda (err)
                           (display-error err)
                           (loop)))
          (rep (lambda ()
                 (let* ((input (read-line))
                        (expr (bard:read-from-string input)))
                   (if (or (eq? expr quit:)
                           (eq? expr q:))
                       (begin
                         (newline)
                         (display "Bard terminated")
                         (newline))
                       (if (eq? expr #!eof)
                           (loop)
                           (let* ((val (%eval expr $bard-toplevel-environment))
                                  (valstr (%as-string val)))
                             (newline)
                             (display valstr)
                             (loop))))))))
      (with-exception-catcher error-handler rep))))


;;; (%eval '((method (x)(+ x 1)) 2) (%initial-bard-environment))