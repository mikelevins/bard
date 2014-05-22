;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fact.scm
;;;; Project:       Bard
;;;; Purpose:       a test of the Bard VM: factorial written in vm code
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $fact-code
  (asm (instruction 'CONST 1000)
       (instruction 'GSET 'n)
       (instruction 'CONST 1)
       (instruction 'GSET 'p)
       (instruction 'CONST 2)
       (instruction 'GREF 'n)
       (instruction 'PRIM 'GNLT 2)
       (instruction 'TGO 17)
       (instruction 'GREF 'n)
       (instruction 'GREF 'p)
       (instruction 'PRIM 'GNMUL 2)
       (instruction 'GSET 'p)
       (instruction 'CONST 1)
       (instruction 'GREF 'n)
       (instruction 'PRIM 'GNSUB 2)
       (instruction 'GSET 'n)
       (instruction 'GO 4)
       (instruction 'GREF 'p) ;; base
       (instruction 'HALT)))

(define $fact
  (make-fn '() #f (default-environment) $fact-code))

(define $vm
  (make-vmstate $fact 0 0 '() '() (default-globals) #f))

;;; (begin (time (vmstart $vm))(showvm $vm))

;;; for comparison:

(define (fact n p)
  (if (< n 2)
      p
      (fact (- n 1)
            (* n p))))

;;; (time (fact 1000 1))
