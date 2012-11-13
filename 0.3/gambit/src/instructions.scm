;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instructions.scm
;;;; Project:       Bard
;;;; Purpose:       bard vm instructions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $instructions (make-vector 11 #f))
(define $opnames (make-vector 11 #f))

(define-macro (^ lambda-list . body)
  `(lambda ,lambda-list ,@body))

(define-macro (%defop iname opcode ifun)
  `(begin
     (define ,iname ,opcode)
     (vector-set! $instructions ,opcode ,ifun)
     (vector-set! $opnames ,opcode ,iname)
     ',iname))

(%defop LREF    0 (^ (vm) (%push! vm (%var-val (%env-ref (%env vm)(%arg1 (%instr vm))(%arg2 (%instr vm)))))))
(%defop LSET    1 (^ (vm) (%env-set! (%env vm)(%arg1 (%instr vm))(%arg2 (%instr vm))(%pop vm))))
(%defop GREF    2 (^ (vm) (%push! vm (%var-val (%global-ref (%globals vm) (%arg1 (%instr vm)))))))
(%defop GSET    3 (^ (vm) (%global-set! (%globals vm) (%arg1 (%instr vm))(%pop vm))))
(%defop POP     4 (^ (vm) (%pop vm)))
(%defop CONST   5 (^ (vm) (%push! vm (%arg1 (%instr vm)))))
(%defop JUMP    6 (^ (vm) (%setpc! vm (%arg1 (%instr vm)))))
(%defop FJUMP   7 (^ (vm) (if (%logically-false? (%pop vm))(%setpc! vm (%arg1 (%instr vm))))))
(%defop TJUMP   8 (^ (vm) (if (%logically-true? (%pop vm))(%setpc! vm (%arg1 (%instr vm))))))

(define (% . args) args)
(define (%op i)(list-ref i 0))
(define (%arg1 i)(list-ref i 1))
(define (%arg2 i)(list-ref i 2))

