;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.scm
;;;; Project:       Bard
;;;; Purpose:       primitive procedures, defined in Scheme, but bound to
;;;;                Bard variables in the initial environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; repl
;;; ---------------------------------------------------------------------

(define prim:read
  (%make-primitive-method
   (lambda (#!optional (in (current-input-port)))
     (bard:read in))
   name: 'read
   required-count: 0
   optional-parameter: #t))

(define prim:eval
  (%make-primitive-method
   (lambda (expr #!optional (env (%null-environment)))
     (%eval expr env))
   name: 'eval
   required-count: 1
   optional-parameter: #t))

(define prim:apply
  (%make-primitive-method
   (lambda (fn args)(%apply fn args))
   name: 'apply
   required-count: 2
   optional-parameter: #f))

(define prim:print
  (%make-primitive-method
   (lambda (thing #!optional (out (current-output-port)))
     (bard:print thing out))
   name: 'print
   required-count: 1
   optional-parameter: #t))

;;; Number
;;; ---------------------------------------------------------------------

(define prim:+
  (%make-primitive-method
   +
   name: '+
   required-count: 0
   optional-parameter: #t))

(define prim:-
  (%make-primitive-method
   -
   name: '-
   required-count: 0
   optional-parameter: #t))

(define prim:*
  (%make-primitive-method
   *
   name: '*
   required-count: 0
   optional-parameter: #t))

(define prim:/
  (%make-primitive-method
   /
   name: '/
   required-count: 0
   optional-parameter: #t))

(define prim:=
  (%make-primitive-method
   =
   name: '=
   required-count: 0
   optional-parameter: #t))

(define prim:>
  (%make-primitive-method
   >
   name: '>
   required-count: 0
   optional-parameter: #t))

(define prim:<
  (%make-primitive-method
   <
   name: '<
   required-count: 0
   optional-parameter: #t))

(define prim:>=
  (%make-primitive-method
   >=
   name: '>=
   required-count: 0
   optional-parameter: #t))

(define prim:<=
  (%make-primitive-method
   <=
   name: '<=
   required-count: 0
   optional-parameter: #t))

(define prim:odd?
  (%make-primitive-method
   odd?
   name: 'odd?
   required-count: 1
   optional-parameter: #f))

(define prim:even?
  (%make-primitive-method
   even?
   name: 'even?
   required-count: 1
   optional-parameter: #f))

;;; List
;;; ---------------------------------------------------------------------

(define prim:list
  (%make-primitive-method
   (lambda args (%cons->ralist args))
   name: 'list
   required-count: 0
   optional-parameter: #t))

;;; Frame
;;; ---------------------------------------------------------------------

(define prim:frame
  (%make-primitive-method
   (lambda args (%make-frame args))
   name: 'frame
   required-count: 0
   optional-parameter: #t))

;;; System 
;;; ---------------------------------------------------------------------

(define prim:gc
  (%make-primitive-method
   ##gc
   name: 'gc
   required-count: 0
   optional-parameter: #f))

(define prim:room
  (%make-primitive-method
   (lambda ()
     (begin
       (gc-report-set! #t)
       (##gc)
       (gc-report-set! #f)))
   name: 'room
   required-count: 0
   optional-parameter: #f))

(define prim:exit
  (%make-primitive-method
   exit
   name: 'exit
   required-count: 0
   optional-parameter: #f))

(define prim:quit
  (%make-primitive-method
   exit
   name: 'quit
   required-count: 0
   optional-parameter: #f))

(define prim:version
  (%make-primitive-method
   (lambda () $bard-version-string)
   name: 'version
   required-count: 0
   optional-parameter: #f))

;;; functions
;;; ---------------------------------------------------------------------

(define prim:complement 
  (%make-primitive-method
   (lambda (f)
     (%make-primitive-method (lambda args (not (%apply f (%cons->ralist args))))
                             required-count: 0
                             optional-parameter: #t))
   name: 'complement
   required-count: 1
   optional-parameter: #f))

(define prim:constantly 
  (%make-primitive-method
   (lambda (c)(%make-primitive-method (lambda args c)
                                      required-count: 0
                                      optional-parameter: #t))
   name: 'constantly
   required-count: 1
   optional-parameter: #f))

(define prim:flip 
  (%make-primitive-method
   (lambda (f)(%make-primitive-method (lambda (x y) (%apply f (%list y x)))
                                      required-count: 2
                                      optional-parameter: #f))
   name: 'flip
   required-count: 1
   optional-parameter: #f))

(define prim:partial 
  (%make-primitive-method
   (lambda (f #!rest outer-args)
     (%make-primitive-method (lambda inner-args (%apply f (%cons->ralist (append outer-args inner-args))))
                             required-count: 0
                             optional-parameter: #t))
   name: 'partial
   required-count: 1
   optional-parameter: #t))

;;; type system
;;; ---------------------------------------------------------------------

(define prim:type? %type?)

(define prim:type?
  (%make-primitive-method
   %type?
   name: 'type?
   required-count: 1
   optional-parameter: #f))

(define prim:type
  (%make-primitive-method
   %object->bard-type
   name: 'type
   required-count: 1
   optional-parameter: #f))

(define prim:singleton?
  (%make-primitive-method
   %singleton?
   name: 'singleton?
   required-count: 1
   optional-parameter: #f))

(define prim:singleton
  (%make-primitive-method
   %singleton
   name: 'singleton
   required-count: 1
   optional-parameter: #f))

