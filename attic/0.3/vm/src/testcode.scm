;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          testcode.scm
;;;; Project:       Bard
;;;; Purpose:       the canonical test program
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define $vmcode
  (vector (vector LREF 0 0)
          (vector CONST 1000000 0)
          (vector GT 0 0)
          (vector FJUMP 9 0)
          (vector LREF 0 0)
          (vector CONST 1 0)
          (vector ADD 0 0)
          (vector LSET 0 0)
          (vector JUMP 0 0)
          (vector HALT 0 0)))

;;; (define $testprog (make-program $vmcode))
;;; (save-bardo $testprog "/Users/mikel/Desktop/testcode.bardo")
;;; (define $program (load-bardo "/Users/mikel/Desktop/testcode.bardo"))
