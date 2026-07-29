;;;; ***********************************************************************
;;;;
;;;; Name:          test.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the conformance ladder
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(defpackage :bard-test
  (:use #:cl #:it.bese.fiveam)
  (:local-nicknames (#:fiveam #:it.bese.fiveam))
  (:export #:run-tests))

(in-package :bard-test)

;;; The tests are the programs from doc/kernel-tutorial.md part 2, in
;;; order. Each stage of part 3 ends with one of them running, so a
;;; failure tells you exactly which stage you are on.
;;;
;;; Every test is individually runnable from the repl by evaluating the
;;; #+repl form beside it.

(def-suite bard-tests
  :description "The kernel conformance ladder.")

(in-suite bard-tests)

(defun run-tests ()
  "Run the whole ladder. Returns T if everything passes."
  (run! 'bard-tests))

#+repl (run-tests)
#+repl (asdf:test-system :bard)

;;; ---------------------------------------------------------------------
;;; helpers
;;; ---------------------------------------------------------------------

(defun run-program (forms &rest args)
  "Assemble and run FORMS, returning the delivered values as a list."
  (bard:run-code (apply #'bard:assemble forms args)))

(defun run-1 (forms &rest args)
  "Assemble and run FORMS, returning the single delivered value."
  (first (apply #'run-program forms args)))

#+repl (run-1 '((CONST 42) (RETURN 1)))
#+repl (bard:disassemble (bard:assemble '((CONST 42) (RETURN 1)) :name "answer"))
#+repl (let ((bard:*trace* t)) (run-program '((CONST 42) (RETURN 1))))

;;; ---------------------------------------------------------------------
;;; stage 2 -- the loop, CONST, RETURN
;;; ---------------------------------------------------------------------

(test |2.1 a constant|
  "CONST, RETURN, and thread termination."
  (is (equal '(42) (run-program '((CONST 42) (RETURN 1)))))
  (is (equal '() (run-program '((RETURN 0)))))
  (is (equal '(1 2 3) (run-program '((CONST 1) (CONST 2) (CONST 3) (RETURN 3))))))

#+repl (run! '|2.1 a constant|)

;;; ---------------------------------------------------------------------
;;; stage 3 -- bindings, primitive calls, receivers
;;; ---------------------------------------------------------------------

(test |2.2 calling a primitive|
  "GLOBAL, CALL's descriptor branch, and a receiver."
  (is (= 5 (run-1 '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV 1) (RETURN 1)))))
  (is (= 6 (run-1 '((CONST 2) (CONST 3) (GLOBAL _fixnum-mul) (CALL 2) (RECV 1) (RETURN 1))))))

#+repl (run! '|2.2 calling a primitive|)

(test |redefinition reaches compiled code|
  "A binding is read at run time, so rebinding + changes what this
already-assembled code calls. This is the whole point of P2."
  (let ((code (bard:assemble '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV 1) (RETURN 1))))
        (binding (bard:global-binding '+))
        (saved nil))
    (setf saved (bard:binding-value binding))
    (unwind-protect
         (progn
           (is (= 5 (first (bard:run-code code))))
           ;; rebind + to subtraction; the same code object now subtracts
           (setf (bard:binding-value binding)
                 (bard:binding-value (bard:global-binding '_fixnum-sub)))
           (is (= -1 (first (bard:run-code code)))))
      (setf (bard:binding-value binding) saved))))

#+repl (run! '|redefinition reaches compiled code|)

(test |receivers reconcile producer and consumer|
  "A primitive delivers one value and a count. RECV adjusts to what the
caller wanted, padding with nothing and discarding extras."
  ;; wants one, gets one
  (is (= 5 (run-1 '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV 1) (RETURN 1)))))
  ;; wants none
  (is (equal '() (run-program '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV 0) (RETURN 0)))))
  ;; wants two, gets one padded with nothing
  (is (equal '(5 nil)
             (run-program '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV 2) (RETURN 2)))))
  ;; collects into a list
  (is (equal '((5))
             (run-program '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2) (RECV-ALL) (RETURN 1))))))

#+repl (run! '|receivers reconcile producer and consumer|)

(test |2.4 sequencing and definition|
  "SET-GLOBAL and DROP. Defining something is an ordinary instruction,
not a special mode. Note that DROP and RECV 0 are not interchangeable:
DROP removes a value already on the stack, RECV 0 discards what a call
returned."
  (is (= 30 (run-1 '((CONST 10) (SET-GLOBAL x) (DROP)
                     (CONST 20) (SET-GLOBAL y) (DROP)
                     (GLOBAL x) (GLOBAL y) (GLOBAL +) (CALL 2) (RECV 1)
                     (RETURN 1))))))

#+repl (run! '|2.4 sequencing and definition|)

(test |an unbound global is an error carrying its faulting pc|
  "Stage 11 turns this into a hook that can define the missing name and
resume. Until then it must at least report the instruction that faulted
-- the one at the pc, not the one after it."
  (let ((code (bard:assemble '((CONST 7) (GLOBAL bar) (CALL 1) (RECV 1) (RETURN 1)))))
    (let ((signalled nil))
      (handler-case (bard:run-code code)
        (bard:bard-error (e)
          (setf signalled t)
          (is (= 1 (bard:bard-error-pc e)))
          (is (not (null (bard:bard-error-frame e))))))
      (is (eq t signalled)))))

#+repl (run! '|an unbound global is an error carrying its faulting pc|)

;;; ---------------------------------------------------------------------
;;; stage 4 -- control
;;; ---------------------------------------------------------------------

(test |2.3 a conditional|
  "GOTO and BRANCH-FALSE, with labels resolved by the assembler."
  (flet ((classify (n)
           (let ((binding (bard:global-binding 'n)))
             (setf (bard:binding-value binding) n
                   (bard:binding-bound? binding) t))
           (run-1 '((GLOBAL n) (CONST 3) (GLOBAL <) (CALL 2) (RECV 1)
                    (BRANCH-FALSE big)
                    (CONST "small") (GOTO done)
                    big (CONST "big")
                    done (RETURN 1)))))
    (is (string= "small" (classify 1)))
    (is (string= "big" (classify 7)))
    (is (string= "big" (classify 3)))))

#+repl (run! '|2.3 a conditional|)

;;; ---------------------------------------------------------------------
;;; the assembler and disassembler
;;; ---------------------------------------------------------------------

(test |disassembly names every opcode and renders every operand|
  "You should never have to look up what a number means."
  (let* ((code (bard:assemble '((CONST 42) (GLOBAL +) (CALL 2) (RECV 1)
                                (GOTO done) done (RETURN 1))
                              :name "sample"))
         (text (with-output-to-string (s) (bard:disassemble code :stream s))))
    (is (search "CONST 42" text))
    (is (search "GLOBAL +" text))
    (is (search "CALL 2" text))
    (is (search "RECV 1" text))
    (is (search "GOTO 5" text))
    (is (search "RETURN 1" text))
    (is (search "sample" text))))

#+repl (run! '|disassembly names every opcode and renders every operand|)
#+repl (bard:disassemble (bard:assemble '((CONST 42) (GLOBAL +) (CALL 2) (RECV 1) (RETURN 1))
                                        :name "sample"))

(test |the assembler rejects wrong operand counts|
  (signals error (bard:assemble '((CONST))))
  (signals error (bard:assemble '((CONST 1 2))))
  (signals error (bard:assemble '((GOTO nowhere)))))

#+repl (run! '|the assembler rejects wrong operand counts|)


;;; ---------------------------------------------------------------------
;;; stage 5 -- frames and calls
;;; ---------------------------------------------------------------------

(defparameter *square*
  (bard:assemble '((LOCAL 0 0) (LOCAL 0 0) (GLOBAL _fixnum-mul) (CALL 2) (RECV 1) (RETURN 1))
                 :name "square" :arity 1)
  "(fn (x) (* x x)) -- note that it has no prologue. CALL already put
the argument in slot 0, because building the frame is the call.")

(defparameter *sub*
  (bard:assemble '((LOCAL 0 0) (LOCAL 0 1) (GLOBAL _fixnum-sub) (CALL 2) (RECV 1) (RETURN 1))
                 :name "sub" :arity 2)
  "Asymmetric on purpose: a symmetric operator would not catch swapped
argument order.")

(defparameter *pair*
  (bard:assemble '((LOCAL 0 0) (LOCAL 0 1) (RETURN 2))
                 :name "pair" :arity 2)
  "Returns both of its arguments, to exercise a bytecode function
delivering more than one value.")

(test |2.5 a function|
  "CLOSE, LOCAL, CALL's function branch, and RETURN into a real parent."
  (is (equal '(49)
             (bard:run-code
              (bard:assemble `((CONST 7) (CLOSE ,*square*) (CALL 1) (RECV 1) (RETURN 1)))))))

#+repl (run! '|2.5 a function|)

(test |arguments arrive in order|
  "The first argument lands in slot 0. Verified with subtraction, since
squaring would be identical either way."
  (is (equal '(7) (bard:run-code
                   (bard:assemble `((CONST 10) (CONST 3) (CLOSE ,*sub*) (CALL 2) (RECV 1) (RETURN 1))))))
  (is (equal '(-7) (bard:run-code
                    (bard:assemble `((CONST 3) (CONST 10) (CLOSE ,*sub*) (CALL 2) (RECV 1) (RETURN 1)))))))

#+repl (run! '|arguments arrive in order|)

(test |calls nest|
  "(square (sub 10 3)) -- each call links a parent and each return
restores it."
  (is (equal '(49)
             (bard:run-code
              (bard:assemble `((CONST 10) (CONST 3) (CLOSE ,*sub*) (CALL 2) (RECV 1)
                               (CLOSE ,*square*) (CALL 1) (RECV 1) (RETURN 1)))))))

#+repl (run! '|calls nest|)

(test |a bytecode function delivers multiple values|
  "Same callee, three receivers. What a call produces and what a caller
wants are independent."
  (flet ((call-pair (receiver)
           (bard:run-code
            (bard:assemble `((CONST 10) (CONST 3) (CLOSE ,*pair*) (CALL 2)
                             ,receiver (RETURN ,(if (eq (first receiver) 'recv)
                                                    (second receiver)
                                                    1)))))))
    (is (equal '(10 3) (call-pair '(RECV 2))))
    (is (equal '(10) (call-pair '(RECV 1))))
    (is (equal '((10 3)) (call-pair '(RECV-ALL))))))

#+repl (run! '|a bytecode function delivers multiple values|)

(test |arity is checked against the code object|
  "And the error names the CALL that faulted, not the instruction after
it -- the same P5 property as an unbound global."
  (let ((signalled nil))
    (handler-case
        (bard:run-code (bard:assemble `((CONST 1) (CLOSE ,*sub*) (CALL 1) (RECV 1) (RETURN 1))))
      (bard:bard-error (e)
        (setf signalled t)
        (is (= 2 (bard:bard-error-pc e)))))
    (is (eq t signalled))))

#+repl (run! '|arity is checked against the code object|)

(test |the assembler rejects fewer locals than arguments|
  (signals error (bard:assemble '((RETURN 0)) :arity 2 :n-locals 1)))

#+repl (run! '|the assembler rejects fewer locals than arguments|)

;;; ---------------------------------------------------------------------
;;; stages 5 through 11 -- not yet implemented
;;; ---------------------------------------------------------------------

(test |unimplemented instructions signal rather than misbehave|
  "The ladder should be visible from a backtrace. LOCAL with a nonzero
level needs the lexical chain, which is stage 6."
  (signals bard:bard-error
    (bard:run-code (bard:assemble '((LOCAL 1 0) (RETURN 1)) :n-locals 1)))
  (signals bard:bard-error
    (bard:run-code (bard:assemble '((YIELD) (RETURN 0))))))

#+repl (run! '|unimplemented instructions signal rather than misbehave|)
