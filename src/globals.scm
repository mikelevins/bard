;;;; ***********************************************************************
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       per-vm global variables
;;;; Author:        mikel evins
;;;; Copyright:     2012-2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; API
;;; ----------------------------------------------------------------------
;;;
;;; vm:make-globals                   ; create a new globals object
;;; vm:init-globals globals           ; initialize built-in global variables
;;; vm:global-bound? globals gname    ; return true if gname is bound, false otherwise
;;; vm:get-global globals gname       ; return the value of the named global
;;; vm:put-global! globals gname gval ; assert gval as the value for variable gname

;;; ----------------------------------------------------------------------
;;; implementation
;;; ----------------------------------------------------------------------
;;; TODO: this implementation is not thread safe. When adding the capability
;;; to execute more than one actor on the same vm, add also a thread-safe
;;; implementation of the globals API.


(define (vm:make-globals)
  (make-table test: eqv?))

(define (vm:init-globals globals)
  ;; TODO: add code to set up the default global variables
  #f)

(define (vm:global-bound? globals gname)
  (and (table-search (lambda (key value)
                       (eqv? key gname))
                     globals)
       #t))

(define (vm:get-global globals gname)
  (table-ref globals gname #!unbound))

(define (vm:put-global! globals gname gval)
  (table-set! globals gname gval))


