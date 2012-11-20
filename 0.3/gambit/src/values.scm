;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; this file defines represenations, constructors, and primitive
;;; predicates for Bard's base built-in values. It does not
;;; implement protocols or library functions.
;;; 
;;; values      schema            class        protocol   Gambit
;;; ---------------------------------------------------------------------
;;; undefined   <undefined>       Undefined    Values     #!unbound
;;; nothing     <null>            Null         Values     '()
;;; true        <true>            Boolean      Boolean    #t
;;; false       <false>           Boolean      Boolean    #f
;;; small ints  <small-integer>   Integer      Numbers    fixnum
;;; big ints    <big-integer>     Integer      Numbers    bignum
;;; characters  <character>       Character    Text       char
;;; keywords    <keyword>         Keyword      Names      keyword
;;; symbols     <symbol>          Symbol       Names      symbol
;;; function    <function>        Function     Applicable procedure
;;; method      <method>          Method       Applicable procedure
;;; protocol    <protocol>        Protocol     Types      define-type
;;; class       <class>           Class        Types      define-type
;;; actor       <actor>           Actor        Actors     define-type
;;; record-type <record-schema>   Schema       Types      define-type
;;; vector-type <vector-schema>   Schema       Types      define-type
;;; (a . b)     <pair>            Tuple        Tuples     pair
;;; tables      <alist-table>     Table        Maps       define-type
;;; (a b c)     <pair>            Table        Maps       pair
;;; "foo"       <string>          Table        Maps       string
;;; #(0 1 2)    <vector>          Vector       Maps       vector
;;; #(0 1 2)    <bytevector>      Vector       Maps       byte-vector
;;; {a: 1 b: 2} <record-instance> Table        Maps       define-type
;;; lists       <alist-table>     List         Lists      define-type
;;; (a b c)     <pair>            List         Lists      pair
;;; "foo"       <string>          List         Lists      string
;;; #(0 1 2)    <vector>          List         Lists      vector
;;; #(0 1 2)    <bytevector>      List         Lists      byte-vector
;;; series      <series>          Series       Series     define-type
;;; input       <input-stream>    InputStream  Streams    input port
;;; output      <output-stream>   OutputStream Streams    output port

;;; ---------------------------------------------------------------------
;;; general utils
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; Gambit runtime utils
;;; ---------------------------------------------------------------------

(define (%procedure-metadata proc)
  (if (procedure? proc)
      (cond
       ((##interp-procedure? proc)(let* ((rte (##interp-procedure-rte proc)))
                                    (if rte (##vector-ref rte 1) #f)))
       ((##closure? proc)(##closure-ref proc 1))
       (else #f))
      #f))

;;; ---------------------------------------------------------------------
;;; <undefined>
;;; ---------------------------------------------------------------------

(define (%undefined) #!unbound)
(define (%undefined? x) (eqv? x #!unbound))
(define (%defined? x) (not (%undefined? x)))

;;; ---------------------------------------------------------------------
;;; <null>
;;; ---------------------------------------------------------------------

(define (%nothing) '())
(define (%nothing? x) (eq? x '()))
(define (%something? x) (not (memq x '(#!unbound ()))))

;;; ---------------------------------------------------------------------
;;; <true> and <false>
;;; ---------------------------------------------------------------------

(define (%true) #t)
(define (%true? x) (eq? x #t))

(define (%false) #f)
(define (%false? x) (eq? x #f))

(define %boolean? boolean?)

(define (%logically-false? x)
  (memv x '(#f '() #!unbound)))

(define (%logically-true? x)
  (not (%logically-false? x)))

;;; ---------------------------------------------------------------------
;;; <small-integer>
;;; ---------------------------------------------------------------------

(define %small-integer? ##fixnum?)

;;; ---------------------------------------------------------------------
;;; <big-integer>
;;; ---------------------------------------------------------------------

(define %big-integer? ##bignum?)

;;; ---------------------------------------------------------------------
;;; <character>
;;; ---------------------------------------------------------------------

(define %unicode-character? char?)

;;; ---------------------------------------------------------------------
;;; <keyword>
;;; ---------------------------------------------------------------------

(define %keyword? keyword?)

;;; ---------------------------------------------------------------------
;;; <symbol>
;;; ---------------------------------------------------------------------

(define %symbol? symbol?)

;;; ---------------------------------------------------------------------
;;; <function>
;;; ---------------------------------------------------------------------

(define-type %function-metadata
  constructor: %private-make-function-metadata
  (debug-name %function-metadata-debug-name)
  (input-types  %function-metadata-input-types)
  (output-types  %function-metadata-output-types)
  (method-table %function-metadata-method-table %set-function-metadata-method-table!))

(define (%make-function-metadata name input-types output-types method-table)
  (%private-make-function-metadata name input-types output-types method-table))

(define (%function? x) 
  (%function-metadata? (%procedure-metadata x)))

(define (%function-metadata f)
  (let ((meta (%procedure-metadata f)))
    (or (and (%function-metadata? meta)
             meta)
        #f)))

(define (%make-method-table)
  (make-table test: equal?))

(define (%make-function-closure meta)
  (let ((meta meta))
    (lambda args (%apply-function meta args))))

(define (%make-function input-types output-types #!key (name #f))
  (let* ((intypes (map identity input-types))
         (outtypes (map identity output-types))
         (name (or name "an anonymous function"))
         (meta (%make-function-metadata name intypes outtypes (%make-method-table))))
    (%make-function-closure meta)))

;;; ---------------------------------------------------------------------
;;; <method>
;;; ---------------------------------------------------------------------

(define (%method? x) 
  (and (procedure? x)
       (not (%function? x))))

;;; ---------------------------------------------------------------------
;;; <protocol>
;;; ---------------------------------------------------------------------

(define-type %protocol
  constructor: %private-make-protocol
  (name %protocol-name)
  (functions %protocol-functions))

(define (%make-protocol name functions-alist)
  (let ((fn-table (list->table functions-alist test: eq?)))
    (%private-make-protocol name fn-table)))

;;; ---------------------------------------------------------------------
;;; <class>
;;; ---------------------------------------------------------------------

(define-type %class
  constructor: %private-make-class
  (name %class-name)
  (members %class-members %set-class-members!))

(define (%make-class name)
  (%private-make-class name '()))

;;; ---------------------------------------------------------------------
;;; <actor>
;;; ---------------------------------------------------------------------

(define-type %actor
  constructor: %private-make-actor)

(define (%make-actor)
  (%private-make-actor))

;;; ---------------------------------------------------------------------
;;; <record-schema>
;;; ---------------------------------------------------------------------

(define-type %record-schema
  constructor: %private-make-record-schema)

(define (%make-record-schema)
  (%private-make-record-schema))

;;; ---------------------------------------------------------------------
;;; <vector-schema>
;;; ---------------------------------------------------------------------

(define-type %vector-schema
  constructor: %private-make-vector-schema)

(define (%make-vector-schema)
  (%private-make-vector-schema))

;;; ---------------------------------------------------------------------
;;; <pair>
;;; ---------------------------------------------------------------------

(define %pair? pair?)

(define (%make-pair a b)(cons a b))
(define %pair-left car)
(define %pair-right cdr)

;;; ---------------------------------------------------------------------
;;; <alist-table>
;;; ---------------------------------------------------------------------

(define-type %alist-table
  constructor: %private-make-alist-table
  (test %alist-table-test)
  (entries %alist-table-entries))

(define (%make-alist-table alist #!key (test equal?))
  (%private-make-alist-table test (copy-tree alist)))

(define (%assoc key tbl)
  (find-association key
                    (%alist-table-entries tbl)
                    test: (%alist-table-test tbl)))

(define (%rmkey key tbl)
  (let* ((old-entries (%alist-table-entries tbl))
         (test (%alist-table-test tbl))
         (new-entries (remove (lambda (e)(test key (car e)))
                              old-entries)))
    (%private-make-alist-table test
                               (cons (cons key val)
                                     new-entries))))

(define (%acons key val tbl)
  (let ((entries (%alist-table-entries tbl)))
    (%private-make-alist-table (%alist-table-test tbl)
                               (cons (cons key val)
                                     entries))))

;;; ---------------------------------------------------------------------
;;; <string>
;;; ---------------------------------------------------------------------

(define %string? string?)

;;; ---------------------------------------------------------------------
;;; <vector>
;;; ---------------------------------------------------------------------

(define %vector? vector?)

;;; ---------------------------------------------------------------------
;;; <bytevector>
;;; ---------------------------------------------------------------------

(define %bytevector? u8vector?)

(define (make-bytevector bytes)
  (list->u8vector bytes))

;;; ---------------------------------------------------------------------
;;; <record-instance>
;;; ---------------------------------------------------------------------

(define-type %record-instance
  constructor: %private-make-record-instance)

(define (%make-record-instance)
  (%private-make-record-instance))

;;; ---------------------------------------------------------------------
;;; <series>
;;; ---------------------------------------------------------------------

(define-type %series
  constructor: %private-make-series)

(define (%make-series)
  (%private-make-series))

;;; ---------------------------------------------------------------------
;;; <input-stream>
;;; ---------------------------------------------------------------------

(define %input-stream? input-port?)

;;; ---------------------------------------------------------------------
;;; <output-stream>
;;; ---------------------------------------------------------------------

(define %output-stream? output-port?)
