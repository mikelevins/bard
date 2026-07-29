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
;;; The assembly programs are defined at toplevel and laid out as
;;; listings, so they read as source rather than as arguments buried
;;; inside a test form. Each test then says only what it expects.
;;;
;;; Every test is individually runnable from the repl by evaluating the
;;; #+repl form beside it.
;;;
;;; Naming: +lap-<instruction>-<case>[-<manner>]+
;;;
;;; The prefix names the instruction under test, so the corpus groups by
;;; machine feature and a grep for +lap-call- finds everything exercising
;;; calls. The optional suffix says how the test works: -error for a
;;; program that faults deliberately, -listing for one that is
;;; disassembled rather than run, -pending for one that asserts an
;;; unimplemented stage still signals. A +lap-fn-...+ is a callee used by
;;; other programs rather than a program in its own right, and
;;; +code-...+ is its assembled form.
;;;
;;; The name says what is tested; the docstring says what it computes.
;;;
;;; These are +constants+ by intention and never rebound, but they are
;;; DEFPARAMETER rather than DEFCONSTANT: DEFCONSTANT on a list re-signals
;;; on every reload, because the new value is not EQL to the old.

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

(defun set-global (name value)
  (let ((binding (bard:global-binding name)))
    (setf (bard:binding-value binding) value
          (bard:binding-bound? binding) t)))

;;; ---------------------------------------------------------------------
;;; assembly -- values and returns
;;; ---------------------------------------------------------------------

(defparameter +lap-const+
  '((CONST 42)
    (RETURN 1))
  "42")

(defparameter +lap-return-none+
  '((RETURN 0))
  "A computation that delivers nothing at all.")

(defparameter +lap-return-three+
  '((CONST 1)
    (CONST 2)
    (CONST 3)
    (RETURN 3))                         ; all three, bottom first
  "(values 1 2 3)")

#+repl (run-program +lap-const+)
#+repl (bard:disassemble (bard:assemble +lap-return-three+ :name "three"))
#+repl (let ((bard:*trace* t)) (run-program +lap-const+))

;;; ---------------------------------------------------------------------
;;; assembly -- primitive calls and receivers
;;; ---------------------------------------------------------------------

(defparameter +lap-call-primitive+
  '((CONST 2)
    (CONST 3)
    (GLOBAL +)                          ; read at run time, not baked
    (CALL 2)
    (RECV 1)                            ; a call delivers values and a count
    (RETURN 1))
  "(+ 2 3)")

(defparameter +lap-call-primitive-direct+
  '((CONST 2)
    (CONST 3)
    (GLOBAL _fixnum-mul)                ; the primitive itself, not the operator
    (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(_fixnum-mul 2 3)")

;;; The same call received four ways. What a call produces and what its
;;; caller wants are independent; the receiver is where they meet.

(defparameter +lap-recv-one+
  '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(+ 2 3), wanting one value")

(defparameter +lap-recv-none+
  '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2)
    (RECV 0)                            ; discards what the call returned
    (RETURN 0))
  "(+ 2 3) for effect")

(defparameter +lap-recv-two-padded+
  '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2)
    (RECV 2)                            ; one arrives; one is padded with nothing
    (RETURN 2))
  "(+ 2 3), wanting two values")

(defparameter +lap-recv-all+
  '((CONST 2) (CONST 3) (GLOBAL +) (CALL 2)
    (RECV-ALL)                          ; collects into a single list
    (RETURN 1))
  "(multiple-value-list (+ 2 3))")

;;; ---------------------------------------------------------------------
;;; assembly -- definition and sequencing
;;; ---------------------------------------------------------------------

(defparameter +lap-set-global-sequence+
  '((CONST 10)
    (SET-GLOBAL x)                      ; SET-GLOBAL does not pop
    (DROP)                              ; ...so the value is dropped here
    (CONST 20)
    (SET-GLOBAL y)
    (DROP)
    (GLOBAL x)
    (GLOBAL y)
    (GLOBAL +)
    (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(begin (set! x 10) (set! y 20) (+ x y))

DROP and RECV 0 are not interchangeable: DROP removes a value already on
the stack, RECV 0 discards what a call returned.")

(defparameter +lap-global-unbound-error+
  '((CONST 7)
    (GLOBAL bar)                        ; unbound -- faults here, at pc 1
    (CALL 1)
    (RECV 1)
    (RETURN 1))
  "(bar 7), where bar has no value.

Stage 11 turns this into a hook that can define bar and resume. Until
then it must at least report the instruction that faulted -- the GLOBAL
at 1, not the CALL at 2.")

;;; ---------------------------------------------------------------------
;;; assembly -- control
;;; ---------------------------------------------------------------------

(defparameter +lap-branch-false-both-arms+
  '((GLOBAL n)
    (CONST 3)
    (GLOBAL <)
    (CALL 2)
    (RECV 1)
    (BRANCH-FALSE big)                  ; the only conditional branch
    (CONST "small")
    (GOTO done)
  big
    (CONST "big")
  done
    (RETURN 1))
  "(if (< n 3) \"small\" \"big\")")

;;; ---------------------------------------------------------------------
;;; assembly -- functions
;;; ---------------------------------------------------------------------

;;; None of these has a prologue. CALL has already placed the arguments
;;; in the low slots, because building the frame is the call.

(defparameter +lap-fn-square+
  '((LOCAL 0 0)                         ; x
    (LOCAL 0 0)                         ; x again
    (GLOBAL _fixnum-mul)
    (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(fn (x) (* x x))")

(defparameter +lap-fn-sub+
  '((LOCAL 0 0)                         ; a -- the first argument, slot 0
    (LOCAL 0 1)                         ; b
    (GLOBAL _fixnum-sub)                ; asymmetric on purpose: a symmetric
    (CALL 2)                            ; operator would not catch a swap
    (RECV 1)
    (RETURN 1))
  "(fn (a b) (- a b))")

(defparameter +lap-fn-pair+
  '((LOCAL 0 0)
    (LOCAL 0 1)
    (RETURN 2))                         ; a bytecode function, two values
  "(fn (a b) (values a b))")

(defparameter +code-square+ (bard:assemble +lap-fn-square+ :name "square" :arity 1))
(defparameter +code-sub+ (bard:assemble +lap-fn-sub+ :name "sub" :arity 2))
(defparameter +code-pair+ (bard:assemble +lap-fn-pair+ :name "pair" :arity 2))

#+repl (bard:disassemble +code-square+)

(defparameter +lap-call-fn+
  `((CONST 7)
    (CLOSE ,+code-square+)              ; captures the current frame
    (CALL 1)
    (RECV 1)
    (RETURN 1))
  "((fn (x) (* x x)) 7)")

(defparameter +lap-call-fn-arg-order+
  `((CONST 10)
    (CONST 3)
    (CLOSE ,+code-sub+)
    (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(- 10 3), which must be 7 and not -7")

(defparameter +lap-call-fn-arg-order-reversed+
  `((CONST 3)
    (CONST 10)
    (CLOSE ,+code-sub+)
    (CALL 2)
    (RECV 1)
    (RETURN 1))
  "(- 3 10)")

(defparameter +lap-call-fn-nested+
  `((CONST 10)
    (CONST 3)
    (CLOSE ,+code-sub+)
    (CALL 2)
    (RECV 1)                            ; 7 stays on the stack as the
    (CLOSE ,+code-square+)              ; argument to the next call
    (CALL 1)
    (RECV 1)
    (RETURN 1))
  "(square (sub 10 3))")

(defparameter +lap-recv-two-from-fn+
  `((CONST 10) (CONST 3) (CLOSE ,+code-pair+) (CALL 2) (RECV 2) (RETURN 2))
  "Both values of a two-value function.")

(defparameter +lap-recv-one-from-fn+
  `((CONST 10) (CONST 3) (CLOSE ,+code-pair+) (CALL 2) (RECV 1) (RETURN 1))
  "The first value only; the second is discarded.")

(defparameter +lap-recv-all-from-fn+
  `((CONST 10) (CONST 3) (CLOSE ,+code-pair+) (CALL 2) (RECV-ALL) (RETURN 1))
  "Both values, as a list.")

(defparameter +lap-call-fn-arity-error+
  `((CONST 1)
    (CLOSE ,+code-sub+)                 ; sub takes two
    (CALL 1)                            ; ...called with one; faults at pc 2
    (RECV 1)
    (RETURN 1))
  "A call whose argument count does not match the callee's code object.")

;;; ---------------------------------------------------------------------
;;; assembly -- the disassembler, and pending stages
;;; ---------------------------------------------------------------------

(defparameter +lap-operand-kinds-listing+
  '((CONST 42)                          ; :const   -- renders the value
    (GLOBAL +)                          ; :binding -- renders the name
    (CALL 2)                            ; :count
    (RECV 1)
    (GOTO done)                         ; :label   -- renders the index
  done
    (RETURN 1))
  "One instruction of each operand kind, for the disassembler.")

(defparameter +lap-local-up-pending+
  '((LOCAL 1 0)                         ; up = 1 needs stage 6
    (RETURN 1))
  "Reaching an enclosing frame. Not implemented yet.")

(defparameter +lap-yield-pending+
  '((YIELD)                             ; needs stage 9
    (RETURN 0))
  "Switching threads. Not implemented yet.")

;;; ---------------------------------------------------------------------
;;; stage 2 -- the loop, CONST, RETURN
;;; ---------------------------------------------------------------------

(test |2.1 a constant|
  "CONST, RETURN, and thread termination."
  (is (equal '(42) (run-program +lap-const+)))
  (is (equal '() (run-program +lap-return-none+)))
  (is (equal '(1 2 3) (run-program +lap-return-three+))))

#+repl (run! '|2.1 a constant|)

;;; ---------------------------------------------------------------------
;;; stage 3 -- bindings, primitive calls, receivers
;;; ---------------------------------------------------------------------

(test |2.2 calling a primitive|
  "GLOBAL, CALL's descriptor branch, and a receiver."
  (is (= 5 (run-1 +lap-call-primitive+)))
  (is (= 6 (run-1 +lap-call-primitive-direct+))))

#+repl (run! '|2.2 calling a primitive|)

(test |redefinition reaches compiled code|
  "A binding is read at run time, so rebinding + changes what this
already-assembled code calls. This is the whole point of P2."
  (let* ((code (bard:assemble +lap-call-primitive+))
         (binding (bard:global-binding '+))
         (saved (bard:binding-value binding)))
    (unwind-protect
         (progn
           (is (= 5 (first (bard:run-code code))))
           (setf (bard:binding-value binding)
                 (bard:binding-value (bard:global-binding '_fixnum-sub)))
           (is (= -1 (first (bard:run-code code)))))
      (setf (bard:binding-value binding) saved))))

#+repl (run! '|redefinition reaches compiled code|)

(test |receivers reconcile producer and consumer|
  "One call, four receivers."
  (is (= 5 (run-1 +lap-recv-one+)))
  (is (equal '() (run-program +lap-recv-none+)))
  (is (equal '(5 nil) (run-program +lap-recv-two-padded+)))
  (is (equal '((5)) (run-program +lap-recv-all+))))

#+repl (run! '|receivers reconcile producer and consumer|)

(test |2.4 sequencing and definition|
  "SET-GLOBAL and DROP. Defining something is an ordinary instruction,
not a special mode."
  (is (= 30 (run-1 +lap-set-global-sequence+))))

#+repl (run! '|2.4 sequencing and definition|)

(test |an unbound global is an error carrying its faulting pc|
  (let ((signalled nil))
    (handler-case (run-program +lap-global-unbound-error+)
      (bard:bard-error (e)
        (setf signalled t)
        (is (= 1 (bard:bard-error-pc e)))
        (is (not (null (bard:bard-error-frame e))))))
    (is (eq t signalled))))

#+repl (run! '|an unbound global is an error carrying its faulting pc|)

;;; ---------------------------------------------------------------------
;;; stage 4 -- control
;;; ---------------------------------------------------------------------

(test |2.3 a conditional|
  "GOTO and BRANCH-FALSE, with labels resolved by the assembler."
  (flet ((classify (n) (set-global 'n n) (run-1 +lap-branch-false-both-arms+)))
    (is (string= "small" (classify 1)))
    (is (string= "big" (classify 7)))
    (is (string= "big" (classify 3)))))

#+repl (run! '|2.3 a conditional|)

;;; ---------------------------------------------------------------------
;;; stage 5 -- frames and calls
;;; ---------------------------------------------------------------------

(test |2.5 a function|
  "CLOSE, LOCAL, CALL's function branch, and RETURN into a real parent."
  (is (equal '(49) (run-program +lap-call-fn+))))

#+repl (run! '|2.5 a function|)

(test |arguments arrive in order|
  "The first argument lands in slot 0. Verified with subtraction, since
squaring would be identical either way."
  (is (equal '(7) (run-program +lap-call-fn-arg-order+)))
  (is (equal '(-7) (run-program +lap-call-fn-arg-order-reversed+))))

#+repl (run! '|arguments arrive in order|)

(test |calls nest|
  "Each call links a parent; each return restores it."
  (is (equal '(49) (run-program +lap-call-fn-nested+))))

#+repl (run! '|calls nest|)

(test |a bytecode function delivers multiple values|
  "Same callee, three receivers."
  (is (equal '(10 3) (run-program +lap-recv-two-from-fn+)))
  (is (equal '(10) (run-program +lap-recv-one-from-fn+)))
  (is (equal '((10 3)) (run-program +lap-recv-all-from-fn+))))

#+repl (run! '|a bytecode function delivers multiple values|)

(test |arity is checked against the code object|
  "The error names the CALL that faulted, not the instruction after it --
the same P5 property as an unbound global."
  (let ((signalled nil))
    (handler-case (run-program +lap-call-fn-arity-error+)
      (bard:bard-error (e)
        (setf signalled t)
        (is (= 2 (bard:bard-error-pc e)))))
    (is (eq t signalled))))

#+repl (run! '|arity is checked against the code object|)

;;; ---------------------------------------------------------------------
;;; the assembler and disassembler
;;; ---------------------------------------------------------------------

(test |disassembly names every opcode and renders every operand|
  "You should never have to look up what a number means."
  (let ((text (with-output-to-string (s)
                (bard:disassemble (bard:assemble +lap-operand-kinds-listing+ :name "sample")
                                  :stream s))))
    (dolist (want '("CONST 42" "GLOBAL +" "CALL 2" "RECV 1" "GOTO 5" "RETURN 1" "sample"))
      (is (search want text)))))

#+repl (run! '|disassembly names every opcode and renders every operand|)
#+repl (bard:disassemble (bard:assemble +lap-operand-kinds-listing+ :name "sample"))

(test |the assembler rejects malformed input|
  (signals error (bard:assemble '((CONST))))
  (signals error (bard:assemble '((CONST 1 2))))
  (signals error (bard:assemble '((GOTO nowhere))))
  (signals error (bard:assemble '((RETURN 0)) :arity 2 :n-locals 1)))

#+repl (run! '|the assembler rejects malformed input|)

;;; ---------------------------------------------------------------------
;;; stages 6 through 11 -- not yet implemented
;;; ---------------------------------------------------------------------

(test |unimplemented instructions signal rather than misbehave|
  "The ladder should be visible from a backtrace."
  (signals bard:bard-error (run-program +lap-local-up-pending+ :n-locals 1))
  (signals bard:bard-error (run-program +lap-yield-pending+)))

#+repl (run! '|unimplemented instructions signal rather than misbehave|)
