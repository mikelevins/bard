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
     (vector-set! $opnames ,opcode ',iname)
     ',iname))

(%defop NOP     0 (^ (vm) (values)))
(%defop HALT    1 (^ (vm) ((%halt vm))))
(%defop LREF    2 (^ (vm) (%push! vm (%var-val (%env-ref (%env vm)(%arg1 (%instr vm))(%arg2 (%instr vm)))))))
(%defop LSET    3 (^ (vm) (%env-set! (%env vm)(%arg1 (%instr vm))(%arg2 (%instr vm))(%pop vm))))
(%defop GREF    4 (^ (vm) (%push! vm (%var-val (%global-ref (%globals vm) (%arg1 (%instr vm)))))))
(%defop GSET    5 (^ (vm) (%global-set! (%globals vm) (%arg1 (%instr vm))(%pop vm))))
(%defop POP     6 (^ (vm) (%pop vm)))
(%defop CONST   7 (^ (vm) (%push! vm (%arg1 (%instr vm)))))
(%defop JUMP    8 (^ (vm) (%setpc! vm (%arg1 (%instr vm)))))
(%defop FJUMP   9 (^ (vm) (if (%logically-false? (%pop vm))(%setpc! vm (%arg1 (%instr vm))))))
(%defop TJUMP  10 (^ (vm) (if (%logically-true? (%pop vm))(%setpc! vm (%arg1 (%instr vm))))))

(define (% . args) args)
(define (%op i)(car i))
(define (%args i)(cdr i))
(define (%arg1 i)(list-ref i 1))
(define (%arg2 i)(list-ref i 2))

(define (%op->opcode op)
  (vector-position op $instructions test: eq?))

(define (%op->opname op)
  (let ((opc (%op->opcode op)))
    (if opc (vector-ref $opnames opc) 'HALT)))

(define (%opcode->op opc)
  (vector-ref $instructions opc))

(define (%instr->string instr)
  (str (%op->opname (%op instr)) " " (%args instr)))
