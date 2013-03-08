;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************


(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; running the vm
;;; ----------------------------------------------------------------------

(define $haltfn #f) ; set by vm startup

(define (vmrun code)
  (display-banner)
  (call/cc
   (lambda (exit)
     (set! $haltfn exit)
     (let loop ((program code)
                (pc 0)
                (vals (make-vector 128 #f))
                (env '())
                (globals (make-table test: eq?)))
       (let* ((instr (vector-ref code pc)))
         (case instr
           ((HALT) ($haltfn))
           ))))))
