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

(define-type gvar name val setter)

(define (default-global-setter val)
  (error "Can't assign to immutable variable"))

(define (make-gvar varname initval #!key (mutable #f))
  (let* ((gv (make-gvar varname initval default-global-setter)))
    (if mutable
        (gvar-setter-set! gv 
                          (lambda (val)
                            (gv-val-set! gv val)
                            val)))
    gv))

(define (make-globals)(make-table test: eq?))

(define (gref globals varname)
  (let ((gv (table-ref globals varname #f)))
    (if gv (gvar-val gv) #!unbound)))

(define (gsetter globals varname)
  (let ((gv (table-ref globals varname #f)))
    (if gv (gvar-setter gv) #!unbound)))

