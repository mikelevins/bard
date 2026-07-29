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

#+repl (run-1 '((const 42) (return 1)))
#+repl (bard:disassemble (bard:assemble '((const 42) (return 1)) :name "answer"))
#+repl (let ((bard:*trace* t)) (run-program '((const 42) (return 1))))

;;; ---------------------------------------------------------------------
;;; stage 2 -- the loop, CONST, RETURN
;;; ---------------------------------------------------------------------

(test |2.1 a constant|
  "CONST, RETURN, and thread termination."
  (is (equal '(42) (run-program '((const 42) (return 1)))))
  (is (equal '() (run-program '((return 0)))))
  (is (equal '(1 2 3) (run-program '((const 1) (const 2) (const 3) (return 3))))))

#+repl (run! '|2.1 a constant|)

;;; ---------------------------------------------------------------------
;;; stage 3 -- bindings, primitive calls, receivers
;;; ---------------------------------------------------------------------

(test |2.2 calling a primitive|
  "GLOBAL, CALL's descriptor branch, and a receiver."
  (is (= 5 (run-1 '((const 2) (const 3) (global +) (call 2) (recv 1) (return 1)))))
  (is (= 6 (run-1 '((const 2) (const 3) (global _fixnum-mul) (call 2) (recv 1) (return 1))))))

#+repl (run! '|2.2 calling a primitive|)

(test |redefinition reaches compiled code|
  "A binding is read at run time, so rebinding + changes what this
already-assembled code calls. This is the whole point of P2."
  (let ((code (bard:assemble '((const 2) (const 3) (global +) (call 2) (recv 1) (return 1))))
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
  (is (= 5 (run-1 '((const 2) (const 3) (global +) (call 2) (recv 1) (return 1)))))
  ;; wants none
  (is (equal '() (run-program '((const 2) (const 3) (global +) (call 2) (recv 0) (return 0)))))
  ;; wants two, gets one padded with nothing
  (is (equal '(5 nil)
             (run-program '((const 2) (const 3) (global +) (call 2) (recv 2) (return 2)))))
  ;; collects into a list
  (is (equal '((5))
             (run-program '((const 2) (const 3) (global +) (call 2) (recv-all) (return 1))))))

#+repl (run! '|receivers reconcile producer and consumer|)

(test |2.4 sequencing and definition|
  "SET-GLOBAL and DROP. Defining something is an ordinary instruction,
not a special mode. Note that DROP and RECV 0 are not interchangeable:
DROP removes a value already on the stack, RECV 0 discards what a call
returned."
  (is (= 30 (run-1 '((const 10) (set-global x) (drop)
                     (const 20) (set-global y) (drop)
                     (global x) (global y) (global +) (call 2) (recv 1)
                     (return 1))))))

#+repl (run! '|2.4 sequencing and definition|)

(test |an unbound global is an error carrying its faulting pc|
  "Stage 11 turns this into a hook that can define the missing name and
resume. Until then it must at least report the instruction that faulted
-- the one at the pc, not the one after it."
  (let ((code (bard:assemble '((const 7) (global bar) (call 1) (recv 1) (return 1)))))
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
                   (bard:binding-boundp binding) t))
           (run-1 '((global n) (const 3) (global <) (call 2) (recv 1)
                    (branch-false big)
                    (const "small") (goto done)
                    big (const "big")
                    done (return 1)))))
    (is (string= "small" (classify 1)))
    (is (string= "big" (classify 7)))
    (is (string= "big" (classify 3)))))

#+repl (run! '|2.3 a conditional|)

;;; ---------------------------------------------------------------------
;;; the assembler and disassembler
;;; ---------------------------------------------------------------------

(test |disassembly names every opcode and renders every operand|
  "You should never have to look up what a number means."
  (let* ((code (bard:assemble '((const 42) (global +) (call 2) (recv 1)
                                (goto done) done (return 1))
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
#+repl (bard:disassemble (bard:assemble '((const 42) (global +) (call 2) (recv 1) (return 1))
                                        :name "sample"))

(test |the assembler rejects wrong operand counts|
  (signals error (bard:assemble '((const))))
  (signals error (bard:assemble '((const 1 2))))
  (signals error (bard:assemble '((goto nowhere)))))

#+repl (run! '|the assembler rejects wrong operand counts|)

;;; ---------------------------------------------------------------------
;;; stages 5 through 11 -- not yet implemented
;;; ---------------------------------------------------------------------

(test |unimplemented instructions signal rather than misbehave|
  "The ladder should be visible from a backtrace."
  (signals bard:bard-error
    (bard:run-code (bard:assemble '((local 0 0) (return 1))))))

#+repl (run! '|unimplemented instructions signal rather than misbehave|)
