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
                (nvals 0)
                (env '())
                (globals (make-table test: eq?)))
       (let* ((instr (vector-ref code pc)))
         (case instr
           ;; HALT
           ((0) ($haltfn))
           ;; LREF
           ((1) (let ((i (%arg1 program))
                      (j (%arg2 program)))
                  (vector-set! vals nvals (%lref env i j))
                  (loop program (+ pc 3) vals (+ nvals 1) env globals)))
           ;; LSET
           ((2))
           ;; GREF
           ((3))
           ;; GSET
           ((4))
           ;; POPV
           ((5))
           ;; CONST
           ((6)) 
           ;; JUMP
           ((7) ) 
           ;; FJUMP
           ((8) ) 
           ;; TJUMP
           ((9) )
           ;; SAVE
           ((10) )
           ;; RETURN
           ((11) )
           ;; CALLJ
           ((12) )
           ;; ARGS
           ((13) )
           ;; ARGS.
           ((14) )
           ;; FN
           ((15) )
           ;; PRIM
           ((16) )
           ;; SETCC
           ((17) )
           ;; CC
           ((18) )
           ;; nullary: VMREAD NEWLINE
           ((19) )
           ;; unary: CAR CDR NOT LIST1 COMPILE DISPLAY WRITE RANDOM
           ((20) )
           ;; binary: ADD SUB MUL DIV LT GT LTE GTE NEQ EQ EQL EQUAL CONS LIST2
           ((21) )
           ;; ternary: LIST3
           ((22) )
           ;; constants: TRUE FALSE NIL ONE- ZERO ONE TWO
           ((23) )))))))
