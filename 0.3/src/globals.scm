;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          environment.scm
;;;; Project:       Bard
;;;; Purpose:       Bard global-variable environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type gvar
  constructor: %private-make-gvar
  name val setter)

(define (default-global-setter val)
  (error "Can't assign to immutable variable"))

(define (make-gvar varname initval #!key (mutable #f))
  (let* ((gv (%private-make-gvar varname initval #f)))
    (if mutable
        (gvar-setter-set! gv 
                          (lambda (val)
                            (gvar-val-set! gv val)
                            val)))
    gv))

(define (make-globals)(make-table test: eq?))

(define (defglobal! globals varname initval #!key (mutable #f))
  (table-set! globals varname (make-gvar varname initval mutable: mutable))
  initval)

(define (gref globals varname)
  (let ((gv (table-ref globals varname #f)))
    (if gv (gvar-val gv) #!unbound)))

(define (gsetter globals varname)
  (let ((gv (table-ref globals varname #f)))
    (if gv (gvar-setter gv) #!unbound)))

#| tests

(define $globals (make-globals))
(defglobal! $globals 'x 1)
(gref $globals 'x)
(gsetter $globals 'x)
(defglobal! $globals 'y 1 mutable: #t)
(gref $globals 'y)
(gsetter $globals 'y)
((gsetter $globals 'y) 2)
(gref $globals 'y)

|#
