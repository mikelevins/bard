;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prim.scm
;;;; Project:       Bard
;;;; Purpose:       bard 0.3 primitives
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type %prim
  constructor: %private-make-prim
  (name %prim-name)
  (required-arguments %prim-required-arguments)
  (rest-arguments? %prim-rest-arguments?)
  (opname %prim-opname)
  (side-effects? %prim-side-effects?)
  (opfn %prim-opfn))

(define (%makeprim #!key
                   (name #f)
                   (required-arguments #f)
                   (rest-arguments #f)
                   (opname #f)
                   (side-effects #f)
                   (opfn #f))
  (assert name "Can't define a primitive without a name")
  (assert required-arguments "Can't define a primitive without defining the number of required arguments")
  (assert opname "Can't define a primitive without defining the name of the VM operation")
  (assert opfn "Can't define a primitive without defining the function that implements it")
  (%private-make-prim name required-arguments rest-arguments opname side-effects opfn))

(define $primitives (make-table test: eq?))

(define (%defprim name #!key
                  (required-arguments #f)
                  (rest-arguments #f)
                  (opname #f)
                  (side-effects #f)
                  (opfn #f))
  (table-set! $primitives name (%makeprim name: name required-arguments: required-arguments
                                          rest-arguments: rest-arguments opname: opname
                                          side-effects: side-effects opfn: opfn)))

(define (%primitive? p)
  (table-ref $primitives p #f))

(%defprim '+ 
          required-arguments: 0
          rest-arguments: #t
          opname: 'ADD
          side-effects: #f
          opfn: +)

(%defprim 'version
          required-arguments: 0
          rest-arguments: #f
          opname: 'VERSION
          side-effects: #f
          opfn: (lambda () $bard-version))
