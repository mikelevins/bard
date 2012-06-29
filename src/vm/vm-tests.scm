;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm-tests.scm
;;;; Project:       Bard
;;;; Purpose:       testins of the Bard vm implementation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "asm.scm")
;;; (include "/Volumes/ymra/Users/mikel/Projects/bard/bard/src/vm/asm.scm")


(define *vm-tests* (make-table test: eqv?))

(define (%assert condition message #!key expected found)
  (if condition
      #t
      (begin
        (newline)
        (display "Assertion failed: ")
        (display message)
        (display " expected: ")
        (display expected)
        (display " found: ")
        (display found)
        #f)))



(define (vm:deftest testid testfn
                    #!key
                    (show-vm #t)
                    (expected-pc #f)
                    (expected-instruction #f)
                    (expected-halt? #t)
                    (expected-stack #f))
  (table-set! *vm-tests* testid
              (lambda ()
                (newline)
                (display "Testing ")
                (display testid)
                (display "...")
                (newline)
                (let* ((out-vm (testfn show-vm: show-vm))
                       (out-pc (vm:pc out-vm))
                       (out-instruction (vm:instruction out-vm))
                       (out-halt (vm:halted? out-vm))
                       (out-stack (vm:stack out-vm))
                       (success? #t))
                  (if expected-pc
                      (set! success?
                            (and success?
                                 (%assert (equal? expected-pc out-pc)
                                          "unexpected pc value"
                                          expected: expected-pc
                                          found: out-pc))))
                  (if expected-instruction
                      (set! success?
                            (and success?
                                 (%assert (equal? expected-instruction out-instruction)
                                          "unexpected instruction"
                                          expected: expected-instruction
                                          found: out-instruction))))
                  (set! success?
                        (and success?
                             (%assert (equal? expected-halt? out-halt)
                                      "unexpected halted state"
                                      expected: expected-halt?
                                      found: out-halt)))
                  (if expected-stack
                      (set! success?
                            (and success?
                                 (%assert (equal? expected-stack out-stack)
                                          "unexpected stack state"
                                          expected: expected-stack
                                          found: out-stack))))
                  (newline)
                  (display "Test ")
                  (display testid)
                  (if success?
                      (display " SUCCEEDED.")
                      (display " FAILED."))
                  (newline)(newline)))))

(define (vm:run-test testid)
  (let ((testfn (table-ref *vm-tests* testid)))
    (testfn)))

;;; HALT
;;; ---------------------------------------------------------------------

(vm:deftest 'HALT
            (lambda (#!key (show-vm #t))
              (let* ((env (vm:add-frame (vm:null-env)
                                        (vm:make-frame '(5))))
                     (code (%asm ((LVAR 0 0)(HALT))))
                     (vm (vm:make-vm fun: (vm:make-method code: code env: '())
                                     env: env)))
                (if show-vm
                    (vm:run-show vm)
                    (vm:run vm))
                vm))
            expected-pc: 2
            expected-stack: '(5))

;;; (vm:run-test 'HALT)

#|

;;; LVAR
;;; ---------------------------------------------------------------------
;;; (test-LVAR)
;;; expected end state:
;;; pc: 2 instr: (HALT) halted: #t stack: (5) 

(define (test-LVAR)
  (let* ((env (vm:add-frame (vm:null-env)
                            (vm:make-frame '(5))))
         (code (%asm ((LVAR 0 0)(HALT))))
         (vm (vm:make-vm fun: (vm:make-method code: code env: '())
                         env: env)))
    (vm:run-show vm)))

;;; LSET
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 5 instr: (HALT) halted: #t stack: (10) 

(define (test-LSET)
  (let* ((env (vm:add-frame (vm:null-env)
                          (vm:make-frame '(5))))
       (code (%asm ((CONST 10)(LSET 0 0)(POP)(LVAR 0 0)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '())
                       env: env)))
  (vm:run-show vm)))

;;; GVAR
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 2 instr: (HALT) halted: #t stack: (101) 

(define (test-GVAR)
  (let* ((code (%asm ((GVAR 'x)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:gset! vm 'x 101)
  (vm:run-show vm)))

;;; GSET
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 5 instr: (HALT) halted: #t stack: (202) 

(define (test-GSET)
  (let* ((code (%asm ((CONST 202)(GSET 'x)(POP)(GVAR 'x)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; POP
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 3 instr: (HALT) halted: #t stack: () 

(define (test-POP)
  (let* ((code (%asm ((CONST 77)(POP)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; CONST
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 2 instr: (HALT) halted: #t stack: (77)

(define (test-CONST)
  (let* ((code (%asm ((CONST 77)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; JUMP
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 3 instr: (HALT) halted: #t stack: () 

(define (test-JUMP)
  (let* ((code (%asm ((JUMP 2)(CONST 99)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; FJUMP
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 5 instr: (HALT) halted: #t stack: () 

(define (test-FJUMP)
  (let* ((code (%asm ((CONST #f)(FJUMP 3)(CONST 'wrong)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; TJUMP
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 5 instr: (HALT) halted: #t stack: () 

(define (test-TJUMP)
  (let* ((code (%asm ((CONST #t)(TJUMP 3)(CONST 'wrong)(HALT))))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

;;; SAVE
;;; ---------------------------------------------------------------------
;;; expected end state:
;;; pc: 5 instr: (HALT) halted: #t stack: () 

(define (test-SAVE)
  (let* ((code (%asm ()))
       (vm (vm:make-vm fun: (vm:make-method code: code env: '()))))
  (vm:run-show vm)))

|#

#| tests


RETURN
CALLJ
ARGS
ARGS.
METH
PRIM
TRUE
FALSE
MINUSONE
ZERO
ONE
TWO
NIL
CONS
CAR
CDR
EQ

|#


