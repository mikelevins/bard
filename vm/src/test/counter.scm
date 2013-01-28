;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          counter.scm
;;;; Project:       Bard
;;;; Purpose:       a simple counter in bard vm bytecode
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(define $counter-code
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
