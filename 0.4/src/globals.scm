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

;;; TODO: these really belong in the standard library
;;;       I define them here in order to remind myself
;;;       how to initialize the globals.
(define (globals:init)
  (globals:def! '+ +)
  (globals:def! '- -)
  (globals:def! '* *)
  (globals:def! '/ /)
  (globals:def! '= =)
  (globals:def! '< <)
  (globals:def! '<= <=)
  (globals:def! '> >)
  (globals:def! '>= >=)
  (globals:def! 'list list)
  (globals:def! 'cons cons)
  (globals:def! 'car car)
  (globals:def! 'cdr cdr)
  )
