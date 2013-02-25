;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.rkt
;;;; Project:       bard 0.4
;;;; Purpose:       bard special forms
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************
#lang bard

(provide def define ensure loop method repeat with-exit)

;;; ---------------------------------------------------------------------
;;; special forms
;;; ---------------------------------------------------------------------

;;; ->
;;;
;;; (-> f1..fk) => fx
;;; ---------------------------------------------------------------------
;;; returns a function FX that accepts k arguments. When applied to k
;;; values, the function yields k values, applying F1 to the first
;;; argument, F2 to the second, and so on. Combines usefully with
;;; cascade, e.g: (cascade (a b c) (-> f1 f2 f3)(-> g1 g2 g3)(-> h1 h2
;;; h3)) => v1 v2 v3 where v1 is (h1 (g1 (f1 a))), and so on for the
;;; other values.


;;; begin
;;; ----------------------------------------------------------------------
;;; use racket begin

;;; cascade
;;; ---------------------------------------------------------------------
;;; (cascade (arg1..argk) f1..fn) => val1..valk
;;;
;;; F1 through FN are all functions that accept K arguments and return
;;; K values. cascade applies F1 to arguments arg1..argk. The K
;;; output values become the inputs to F2. F2's outputs are the inputs
;;; to F3, and so on. The outputs of FN are VAL1..VALK

;;; cond
;;; ----------------------------------------------------------------------
;;; use racket cond

;;; def
;;; ----------------------------------------------------------------------

(define-syntax-rule (def var expr) 
  (%define var expr))

;;; define
;;; ----------------------------------------------------------------------

(define-syntax define
  (syntax-rules (class macro method protocol record variable tuple variable)
    ((_ class cname)(%define cname 'cname))
    ((_ variable v expr)(%define v expr))))


;;; ensure
;;; ----------------------------------------------------------------------

(define-syntax-rule (ensure before during after) 
  (dynamic-wind (^ () before)
                (^ () during)
                (^ () after)))

;;; function
;;; ----------------------------------------------------------------------


;;; generate
;;; ----------------------------------------------------------------------
;;; racket: generator


;;; gather
;;; ----------------------------------------------------------------------



;;; if
;;; ----------------------------------------------------------------------
;;; racket: if

;;; let
;;; ----------------------------------------------------------------------


;;; loop
;;; ----------------------------------------------------------------------

(define-syntax-rule (loop loop-name ((var val)...) expr...) 
  (%let loop-name ((var val)...) expr...))


;;; match
;;; ----------------------------------------------------------------------


;;; method
;;; ----------------------------------------------------------------------

(define-syntax-rule (method args expr...) 
  (^ args expr...))


;;; receive
;;; ----------------------------------------------------------------------


;;; repeat
;;; ----------------------------------------------------------------------

(define-syntax-rule (repeat expr...) 
  (%let %_loopvar ()
        (begin expr...)
        (%_loopvar)))


;;; send
;;; ----------------------------------------------------------------------


(define-syntax-rule (with-exit (exit-name) expr...) 
  (call/cc
   (^ (exit-name)
      expr...)))


;;; set!
;;; ----------------------------------------------------------------------


;;; the
;;; ---------------------------------------------------------------------
;;; (the type expr)
;;;
;;; returns expr, constrained to be represented by an instance of type.
;;; if type is a class, and expr is an expression that can be interpreted
;;; as an instance of some type that is a member of the class, then
;;; the result is a value equivalent to expr of some arbitrary conforming
;;; type. if type is a schema and expr designates a value that the 
;;; schema can represent, then that value is returned. otherwise a 
;;; type error is signaled.
;;;
;;; used to specify a representation for a value. ordinarily, bard is
;;; free to choose any conforming representation for the value of an
;;; expression.

;;; time
;;; ----------------------------------------------------------------------


;;; undefine
;;; ----------------------------------------------------------------------


;;; unless
;;; ----------------------------------------------------------------------
;;; racket: unless

;;; values
;;; ----------------------------------------------------------------------


;;; when
;;; ----------------------------------------------------------------------
;;; racket: when

;;; with-open
;;; ----------------------------------------------------------------------
;;; (with-open (var designator {direction: 'input
;;;                             element-type: <character>
;;;                             mode: 'append})
;;;            (do-stuff-to-stream var))
;;;
;;; designator may be one of:
;;; - a url, identifying a file or port
;;; - a sequence, treated as a data source or sink (elements of the sequence
;;;   must be instances of the element-type)
;;; - a stream, treated as a data source or sink (elements of the sequence
;;;   must be instances of the element-type, and the stream IO direction
;;;   must agree with the direction parameter)
;;; - a generator, treated as a data source (elements of the sequence
;;;   must be instances of the element-type, and the stream IO direction
;;;   must be 'input)
;;; - a gatherer, treated as a data sink (elements of the sequence
;;;   must be instances of the element-type, and the stream IO direction
;;;   must be 'output)


