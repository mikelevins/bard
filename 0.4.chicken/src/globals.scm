;;;; ***********************************************************************
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       representation of global variables
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(use srfi-69)

(define *globals* (make-hash-table))
(define +unbound-key+ (cons '() '()))

(define (globals:bound? name)
  (let* ((found (hash-table-ref/default *globals* name +unbound-key+)))
    (if (eq? found +unbound-key+)
        #f
        #t)))

(define (globals:ref name)
  (hash-table-ref/default *globals* name +unbound-key+))

(define (globals:set! name val)
  (if (globals:bound? name)
      (hash-table-set! *globals* name val)
      (error "Undefined variable" name)))

(define (globals:def! name val)
  (hash-table-set! *globals* name val))

;;; TODO: these really belong in the standard library
;;;       I define them here in order to remind myself
;;;       how to initialize the globals.
(define (globals:init)
  (globals:def! 'nothing '())
  (globals:def! 'true #t)
  (globals:def! 'false #f)
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
  (globals:def! 'append append)
  (globals:def! 'cons cons)
  (globals:def! 'car car)
  (globals:def! 'cdr cdr)
  (globals:def! 'nothing? null?)
  (globals:def! 'error error)
  )
