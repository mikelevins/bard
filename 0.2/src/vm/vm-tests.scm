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



(define (vm:deftest testid testcode
                    #!key
                    (env '())
                    (show-vm #t)
                    (expected-pc #f)
                    (expected-env (env:null))
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
                (let* ((testfn (lambda (#!key (show-vm #t))
                                 (let* ((code testcode)
                                        (vm (vm:make-vm method: (vm:make-method code: code env: '())
                                                        env: env)))
                                   (vm:run-show vm)
                                   vm)))
                       (out-vm (testfn show-vm: show-vm))
                       (out-pc (vm:pc out-vm))
                       (out-env (vm:env out-vm))
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
                  (if expected-env
                      (set! success?
                            (and success?
                                 (%assert (equal? expected-env out-env)
                                          "unexpected environment state"
                                          expected: expected-env
                                          found: out-env))))
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
            (%asm ((HALT)))
            expected-pc: 1
            expected-stack: '())

;;; (vm:run-test 'HALT)

;;; LVAR
;;; ---------------------------------------------------------------------

(vm:deftest 'LVAR
            (%asm ((LVAR 0 0)(HALT)))
            env: (vm:add-frame (vm:null-env)(vm:make-frame '(5)))
            expected-pc: 2
            expected-stack: '(5))

;;; (vm:run-test 'LVAR)

;;; LSET
;;; ---------------------------------------------------------------------

(vm:deftest 'LSET
            (%asm ((CONST 10)(LSET 0 0)(POP)(LVAR 0 0)(HALT)))
            env: (vm:add-frame (vm:null-env)(vm:make-frame '(5)))
            expected-pc: 5
            expected-stack: '(10))

;;; (vm:run-test 'LSET)

;;; CONST
;;; ---------------------------------------------------------------------

(vm:deftest 'CONST
            (%asm ((CONST 77)(HALT)))
            expected-pc: 2
            expected-stack: '(77))

;;; (vm:run-test 'CONST)

;;; GSET, GVAR
;;; ---------------------------------------------------------------------

(vm:deftest 'GVAR
            (%asm ((CONST 101)(GSET 'x)(GVAR 'x)(HALT)))
            expected-pc: 4
            expected-stack: '(101 101))

;;; (vm:run-test 'GVAR)

;;; POP
;;; ---------------------------------------------------------------------

(vm:deftest 'POP
            (%asm ((CONST 77)(POP)(HALT)))
            expected-pc: 3
            expected-stack: '())

;;; (vm:run-test 'POP)

;;; JUMP
;;; ---------------------------------------------------------------------

(vm:deftest 'JUMP
            (%asm ((JUMP 2)(CONST 99)(HALT)))
            expected-pc: 3
            expected-stack: '())

;;; (vm:run-test 'JUMP)

;;; FJUMP
;;; ---------------------------------------------------------------------

(vm:deftest 'FJUMP
            (%asm ((CONST #f)(FJUMP 3)(CONST 'wrong)(HALT)))
            expected-pc: 4
            expected-stack: '())

;;; (vm:run-test 'FJUMP)

;;; TJUMP
;;; ---------------------------------------------------------------------

(vm:deftest 'TJUMP
            (%asm ((CONST #t)(TJUMP 3)(CONST 'wrong)(HALT)))
            expected-pc: 4
            expected-stack: '())

;;; (vm:run-test 'TJUMP)

;;; TRUE
;;; ---------------------------------------------------------------------

(vm:deftest 'TRUE
            (%asm ((TRUE)(HALT)))
            expected-pc: 2
            expected-stack: '(#t))

;;; (vm:run-test 'TRUE)

;;; FALSE
;;; ---------------------------------------------------------------------

(vm:deftest 'FALSE
            (%asm ((FALSE)(HALT)))
            expected-pc: 2
            expected-stack: '(#f))

;;; (vm:run-test 'FALSE)

;;; MINUSONE
;;; ---------------------------------------------------------------------

(vm:deftest 'MINUSONE
            (%asm ((MINUSONE)(HALT)))
            expected-pc: 2
            expected-stack: '(-1))

;;; (vm:run-test 'MINUSONE)

;;; ZERO
;;; ---------------------------------------------------------------------

(vm:deftest 'ZERO
            (%asm ((ZERO)(HALT)))
            expected-pc: 2
            expected-stack: '(0))

;;; (vm:run-test 'ZERO)

;;; ONE
;;; ---------------------------------------------------------------------

(vm:deftest 'ONE
            (%asm ((ONE)(HALT)))
            expected-pc: 2
            expected-stack: '(1))

;;; (vm:run-test 'ONE)

;;; TWO
;;; ---------------------------------------------------------------------

(vm:deftest 'TWO
            (%asm ((TWO)(HALT)))
            expected-pc: 2
            expected-stack: '(2))

;;; (vm:run-test 'TWO)

;;; NIL
;;; ---------------------------------------------------------------------

(vm:deftest 'NIL
            (%asm ((NIL)(HALT)))
            expected-pc: 2
            expected-stack: '(()))

;;; (vm:run-test 'NIL)

;;; CONS
;;; ---------------------------------------------------------------------

(vm:deftest 'CONS
            (%asm ((CONST 1)(CONST 2)(CONS)(HALT)))
            expected-pc: 4
            expected-stack: '((1 . 2)))

;;; (vm:run-test 'CONS)

;;; CAR
;;; ---------------------------------------------------------------------

(vm:deftest 'CAR
            (%asm ((CONST 1)(CONST 2)(CONS)(CAR)(HALT)))
            expected-pc: 5
            expected-stack: '(1))

;;; (vm:run-test 'CAR)

;;; CDR
;;; ---------------------------------------------------------------------

(vm:deftest 'CDR
            (%asm ((CONST 1)(CONST 2)(CONS)(CDR)(HALT)))
            expected-pc: 5
            expected-stack: '(2))

;;; (vm:run-test 'CDR)

;;; EQ
;;; ---------------------------------------------------------------------

(vm:deftest 'EQ
            (%asm ((CONST 1)(CONST 1)(EQ)(HALT)))
            expected-pc: 4
            expected-stack: '(#t))

;;; (vm:run-test 'EQ)

;;; METH
;;; ---------------------------------------------------------------------

(define $m1 (vm:make-method code: (%asm ((HALT))) env: (env:null)))

(vm:deftest 'METH
            (%asm ((METH $m1)(HALT)))
            expected-pc: 2)

;;; (vm:run-test 'METH)

;;; PRIM
;;; ---------------------------------------------------------------------

(vm:deftest 'PRIM
            (%asm ((CONST 3)(CONST 6)(NARGS 2)(PRIM *)(HALT)))
            expected-pc: 5
            expected-stack: '(18))

;;; (vm:run-test 'PRIM)

;;; ARGS
;;; ---------------------------------------------------------------------

(vm:deftest 'ARGS
            (%asm ((CONST 1)(CONST 2)(CONST 3)(NARGS 3)(ARGS 3)(HALT)))
            expected-pc: 6
            expected-env: (list (vector 1 2 3)))

;;; (vm:run-test 'ARGS)

;;; ARGS
;;; ---------------------------------------------------------------------

(vm:deftest 'ARGS.
            (%asm ((CONST 1)(CONST 2)(CONST 3)(CONST 4)(CONST 5)(NARGS 5)(ARGS. 3)(HALT)))
            expected-pc: 8
            expected-env: (list (vector 1 2 3 (list 4 5))))

;;; (vm:run-test 'ARGS.)

#| tests

RETURN
CALLJ

|#


