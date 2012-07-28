;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          function.scm
;;;; Project:       Bard VM
;;;; Purpose:       functions and methods
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; applicable objects
;;; ---------------------------------------------------------------------

(define-type applicable
  id: D4A1F712-A2DA-4200-8246-D414880DFB91
  extender: define-applicable)

;;; ---------------------------------------------------------------------
;;; primitives
;;; ---------------------------------------------------------------------

(define-applicable primitive
  id: A1A45308-AF69-4BB7-B975-7CA127118543
  constructor: %private-make-primitive
  name
  nargs
  fn)

(define (make-primitive name nargs fn)
  (%private-make-primitive name nargs fn))

(define $primitives (make-table test: eq?))

(define (%primitive? expr)
  (table-ref $primitives expr #f))

(define (apply-primitive prim args)
  (let* ((argcount (length args))
         (required-argcount (primitive-nargs prim)))
    (cond
     ((eqv? required-argcount #f))
     ((eqv? required-argcount #t))
     ((number? required-argcount)(if (= argcount required-argcount)
                                     (let ((fn (primitive-fn prim)))
                                       (apply fn args))
                                     (error (string-append
                                             "Wrong number of arguments to primitive: " 
                                             (object-string prim)
                                             "; requires "
                                             (object-string required-argcount)
                                             ", but found "
                                             (object-string argcount)))))
     (else (error (string-append "Invalid primitive: " 
                                 (object-string prim)))))))

;;; ---------------------------------------------------------------------
;;; primitives defined
;;; ---------------------------------------------------------------------

(table-set! $primitives 'PRIM+ (make-primitive 'PRIM+ #t (lambda args (apply + args))))
(table-set! $primitives 'PRIM- (make-primitive 'PRIM- #t (lambda args (apply - args))))
(table-set! $primitives 'PRIM* (make-primitive 'PRIM* #t (lambda args (apply * args))))
(table-set! $primitives 'PRIM/ (make-primitive 'PRIM/ #t (lambda args (apply / args))))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-applicable method
  id: 40DF0A42-1D4D-4282-A931-F631D10A32F7
  constructor: %private-make-method
  lambda-list
  code
  env
  debug-name)

(define (make-method lambda-list body-code env #!key (debug-name #f))
  (%private-make-method lambda-list body-code env debug-name))

(define (apply-method f args)
  #f)

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-applicable function
  id: 1C9883E1-FEAC-4EEA-960F-5056AC363A01
  constructor: %private-make-function
  code
  debug-name)

(define (make-function code #!key (debug-name #f))
  (%private-make-function code debug-name))

(define (apply-function f args)
  #f)

;;; ---------------------------------------------------------------------
;;; funcall
;;; ---------------------------------------------------------------------


(define (apply-applicable app args)
  (cond
   ((procedure? f)(apply f args))
   ((function? f)(apply-function f args))
   ((method? f)(apply-method f args))
   ((primitive? f)(apply-primitive f args))
   (else (error (string-append "Not an applicable object: "
                               (object->string app))))))

(define (%funcall f . args)
  (apply-applicable f args))
