;;;; ***********************************************************************
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       representation of global variables
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define *globals* (make-table test: eqv?))

(define (globals:bound? name)
  (let* ((none (gensym 'none)))
    (not (eq? none
              (table-ref *globals* name none)))))

(define (globals:ref name)
  (table-ref *globals*
             name
             #!unbound))

(define (globals:set! name val)
  (table-set! *globals*
              name
              val))

(define (globals:def! name val)
  (table-set! *globals*
              name
              val))

(define (globals:init)
  (globals:def! '+ +)
  (globals:def! '- -)
  (globals:def! '* *)
  (globals:def! '/ /)
  )
