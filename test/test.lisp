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

;;; The programs are in construction order: each stage of building the
;;; machine ends with one of them running, so a failure tells you exactly
;;; which stage you are on.
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

#+repl (run-tests)              ; => T when the whole ladder passes
#+repl (asdf:test-system :bard) ; => T

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
  '((op_CONST 42)
    (op_RETURN 1))
  "42")

(defparameter +lap-return-none+
  '((op_RETURN 0))
  "A computation that delivers nothing at all.")

(defparameter +lap-return-three+
  '((op_CONST 1)
    (op_CONST 2)
    (op_CONST 3)
    (op_RETURN 3))                         ; all three, bottom first
  "(values 1 2 3)")

#+repl (run-program +lap-const+)        ; => (42)
#+repl (run-program +lap-return-three+) ; => (1 2 3)
#+repl (bard:disassemble (bard:assemble +lap-return-three+ :name "three"))
       ; prints the listing; returns no values
#+repl (let ((bard:*trace* t)) (run-program +lap-const+))
       ; => (42), with each instruction traced to *trace-output*

;;; ---------------------------------------------------------------------
;;; assembly -- primitive calls and receivers
;;; ---------------------------------------------------------------------

(defparameter +lap-call-primitive+
  '((op_CONST 2)
    (op_CONST 3)
    (op_GLOBAL +)                          ; read at run time, not baked
    (op_CALL 2)
    (op_RECV 1)                            ; a call delivers values and a count
    (op_RETURN 1))
  "(+ 2 3)")

(defparameter +lap-call-primitive-direct+
  '((op_CONST 2)
    (op_CONST 3)
    (op_GLOBAL _fixnum-mul)                ; the primitive itself, not the operator
    (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(_fixnum-mul 2 3)")

;;; The same call received four ways. What a call produces and what its
;;; caller wants are independent; the receiver is where they meet.

(defparameter +lap-recv-one+
  '((op_CONST 2) (op_CONST 3) (op_GLOBAL +) (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(+ 2 3), wanting one value")

(defparameter +lap-recv-none+
  '((op_CONST 2) (op_CONST 3) (op_GLOBAL +) (op_CALL 2)
    (op_RECV 0)                            ; discards what the call returned
    (op_RETURN 0))
  "(+ 2 3) for effect")

(defparameter +lap-recv-two-padded+
  '((op_CONST 2) (op_CONST 3) (op_GLOBAL +) (op_CALL 2)
    (op_RECV 2)                            ; one arrives; one is padded with nothing
    (op_RETURN 2))
  "(+ 2 3), wanting two values")

(defparameter +lap-recv-all+
  '((op_CONST 2) (op_CONST 3) (op_GLOBAL +) (op_CALL 2)
    (op_RECV-ALL)                          ; collects into a single list
    (op_RETURN 1))
  "(multiple-value-list (+ 2 3))")

;;; ---------------------------------------------------------------------
;;; assembly -- definition and sequencing
;;; ---------------------------------------------------------------------

(defparameter +lap-set-global-sequence+
  '((op_CONST 10)
    (op_SET-GLOBAL x)                      ; op_SET-GLOBAL does not pop
    (op_DROP)                              ; ...so the value is dropped here
    (op_CONST 20)
    (op_SET-GLOBAL y)
    (op_DROP)
    (op_GLOBAL x)
    (op_GLOBAL y)
    (op_GLOBAL +)
    (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(begin (set! x 10) (set! y 20) (+ x y))

op_DROP and op_RECV 0 are not interchangeable: op_DROP removes a value already on
the stack, op_RECV 0 discards what a call returned.")

(defparameter +lap-global-unbound-error+
  '((op_CONST 7)
    (op_GLOBAL bar)                        ; unbound -- faults here, at pc 1
    (op_CALL 1)
    (op_RECV 1)
    (op_RETURN 1))
  "(bar 7), where bar has no value.

Stage 11 turns this into a hook that can define bar and resume. Until
then it must at least report the instruction that faulted -- the op_GLOBAL
at 1, not the op_CALL at 2.")

;;; ---------------------------------------------------------------------
;;; assembly -- control
;;; ---------------------------------------------------------------------

(defparameter +lap-branch-false-both-arms+
  '((op_GLOBAL n)
    (op_CONST 3)
    (op_GLOBAL <)
    (op_CALL 2)
    (op_RECV 1)
    (op_BRANCH-FALSE big)                  ; the only conditional branch
    (op_CONST "small")
    (op_GOTO done)
  big
    (op_CONST "big")
  done
    (op_RETURN 1))
  "(if (< n 3) \"small\" \"big\")")

;;; ---------------------------------------------------------------------
;;; assembly -- functions
;;; ---------------------------------------------------------------------

;;; None of these has a prologue. op_CALL has already placed the arguments
;;; in the low slots, because building the frame is the call.

(defparameter +lap-fn-square+
  '((op_LOCAL 0 0)                         ; x
    (op_LOCAL 0 0)                         ; x again
    (op_GLOBAL _fixnum-mul)
    (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(fn (x) (* x x))")

(defparameter +lap-fn-sub+
  '((op_LOCAL 0 0)                         ; a -- the first argument, slot 0
    (op_LOCAL 0 1)                         ; b
    (op_GLOBAL _fixnum-sub)                ; asymmetric on purpose: a symmetric
    (op_CALL 2)                            ; operator would not catch a swap
    (op_RECV 1)
    (op_RETURN 1))
  "(fn (a b) (- a b))")

(defparameter +lap-fn-pair+
  '((op_LOCAL 0 0)
    (op_LOCAL 0 1)
    (op_RETURN 2))                         ; a bytecode function, two values
  "(fn (a b) (values a b))")

(defparameter +code-square+ (bard:assemble +lap-fn-square+ :name "square" :arity 1))
(defparameter +code-sub+ (bard:assemble +lap-fn-sub+ :name "sub" :arity 2))
(defparameter +code-pair+ (bard:assemble +lap-fn-pair+ :name "pair" :arity 2))

#+repl (bard:disassemble +code-square+) ; prints the listing

(defparameter +lap-call-fn+
  `((op_CONST 7)
    (op_CLOSE ,+code-square+)              ; captures the current frame
    (op_CALL 1)
    (op_RECV 1)
    (op_RETURN 1))
  "((fn (x) (* x x)) 7)")

(defparameter +lap-call-fn-arg-order+
  `((op_CONST 10)
    (op_CONST 3)
    (op_CLOSE ,+code-sub+)
    (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(- 10 3), which must be 7 and not -7")

(defparameter +lap-call-fn-arg-order-reversed+
  `((op_CONST 3)
    (op_CONST 10)
    (op_CLOSE ,+code-sub+)
    (op_CALL 2)
    (op_RECV 1)
    (op_RETURN 1))
  "(- 3 10)")

(defparameter +lap-call-fn-nested+
  `((op_CONST 10)
    (op_CONST 3)
    (op_CLOSE ,+code-sub+)
    (op_CALL 2)
    (op_RECV 1)                            ; 7 stays on the stack as the
    (op_CLOSE ,+code-square+)              ; argument to the next call
    (op_CALL 1)
    (op_RECV 1)
    (op_RETURN 1))
  "(square (sub 10 3))")

(defparameter +lap-recv-two-from-fn+
  `((op_CONST 10) (op_CONST 3) (op_CLOSE ,+code-pair+) (op_CALL 2) (op_RECV 2) (op_RETURN 2))
  "Both values of a two-value function.")

(defparameter +lap-recv-one-from-fn+
  `((op_CONST 10) (op_CONST 3) (op_CLOSE ,+code-pair+) (op_CALL 2) (op_RECV 1) (op_RETURN 1))
  "The first value only; the second is discarded.")

(defparameter +lap-recv-all-from-fn+
  `((op_CONST 10) (op_CONST 3) (op_CLOSE ,+code-pair+) (op_CALL 2) (op_RECV-ALL) (op_RETURN 1))
  "Both values, as a list.")

(defparameter +lap-call-fn-arity-error+
  `((op_CONST 1)
    (op_CLOSE ,+code-sub+)                 ; sub takes two
    (op_CALL 1)                            ; ...called with one; faults at pc 2
    (op_RECV 1)
    (op_RETURN 1))
  "A call whose argument count does not match the callee's code object.")

;;; ---------------------------------------------------------------------
;;; assembly -- the lexical chain
;;; ---------------------------------------------------------------------

(defparameter +lap-set-local+
  '((op_LOCAL 0 0)                         ; x
    (op_CONST 1)
    (op_GLOBAL +)
    (op_CALL 2)
    (op_RECV 1)
    (op_SET-LOCAL 0 0)                     ; x := x+1; does not pop
    (op_DROP)                              ; ...so discard the value here
    (op_LOCAL 0 0)                         ; read x back out of the slot
    (op_RETURN 1))
  "(fn (x) (set! x (+ x 1)) x)")

(defparameter +code-set-local+
  (bard:assemble +lap-set-local+ :name "inc" :arity 1))

(defparameter +lap-fn-bump+
  '((op_LOCAL 1 0)                         ; n -- one level out, in the frame
    (op_CONST 1)                           ; make-counter was running in
    (op_GLOBAL +)
    (op_CALL 2)
    (op_RECV 1)
    (op_SET-LOCAL 1 0)                     ; n := n+1, and leave it as the result
    (op_RETURN 1))
  "(fn () (set! n (+ n 1)))")

(defparameter +code-bump+
  (bard:assemble +lap-fn-bump+ :name "bump" :arity 0))

(defparameter +lap-fn-make-counter+
  `((op_CLOSE ,+code-bump+)                ; captures this frame, where n lives
    (op_RETURN 1))
  "(fn (n) (fn () (set! n (+ n 1))))")

(defparameter +code-make-counter+
  (bard:assemble +lap-fn-make-counter+ :name "make-counter" :arity 1))

(defparameter +lap-counters-independent+
  `((op_CONST 10)
    (op_CLOSE ,+code-make-counter+)
    (op_CALL 1) (op_RECV 1)
    (op_SET-GLOBAL c1) (op_DROP)              ; one counter, starting at 10

    (op_CONST 100)
    (op_CLOSE ,+code-make-counter+)
    (op_CALL 1) (op_RECV 1)
    (op_SET-GLOBAL c2) (op_DROP)              ; another, starting at 100

    (op_GLOBAL c1) (op_CALL 0) (op_RECV 1)       ; 11
    (op_GLOBAL c1) (op_CALL 0) (op_RECV 1)       ; 12
    (op_GLOBAL c2) (op_CALL 0) (op_RECV 1)       ; 101 -- c1 is undisturbed
    (op_GLOBAL c1) (op_CALL 0) (op_RECV 1)       ; 13
    (op_RETURN 4))
  "Two counters from two calls to make-counter.

Their variables are separate because the two frames were separate
computations, not because of any closure machinery.")

;;; ---------------------------------------------------------------------
;;; assembly -- tail calls
;;; ---------------------------------------------------------------------

(defparameter +lap-tailcall+
  `((op_CONST 7)
    (op_CLOSE ,+code-square+)
    (op_TAILCALL 1))                       ; nothing follows, deliberately
  "square, tail-called.

Nothing follows the op_TAILCALL, so this only works if the callee returns
past this frame to its parent. Were op_TAILCALL to link a parent the way
op_CALL does, square would return here -- to a pc past the end of the
code.")

(defparameter +lap-tailcall-primitive+
  `((op_CONST 2)
    (op_CONST 3)
    (op_GLOBAL +)
    (op_TAILCALL 2))
  "(+ 2 3) in tail position.

A primitive has no frame of its own; its values are delivered where this
frame's return would have gone.")

(defparameter +lap-fn-countdown+
  '((op_LOCAL 0 0)
    (op_CONST 0)
    (op_GLOBAL =)
    (op_CALL 2)
    (op_RECV 1)
    (op_BRANCH-FALSE recur)
    (op_CONST done)
    (op_RETURN 1)
  recur
    (op_LOCAL 0 0)
    (op_CONST 1)
    (op_GLOBAL -)
    (op_CALL 2)
    (op_RECV 1)
    (op_GLOBAL countdown)
    (op_TAILCALL 1))                       ; no receiver: it does not return here
  "(fn (n) (if (= n 0) 'done (countdown (- n 1))))")

(defparameter +code-countdown+
  (bard:assemble +lap-fn-countdown+ :name "countdown" :arity 1))

(defparameter +lap-tailcall-deep+
  `((op_CLOSE ,+code-countdown+)
    (op_SET-GLOBAL countdown) (op_DROP)
    (op_CONST 100000)
    (op_GLOBAL countdown)
    (op_CALL 1)
    (op_RECV 1)
    (op_RETURN 1))
  "A hundred thousand tail calls.

Measured: the deepest parent chain reached is 2 -- countdown's frame and
this one -- for n of 10, 1000, and 100000 alike. Constant, not merely
bounded.")

;;; ---------------------------------------------------------------------
;;; assembly -- threads
;;; ---------------------------------------------------------------------

(defparameter +lap-fn-logger+
  '((op_LOCAL 1 0)                      ; tag, from the frame make-logger
    (op_GLOBAL log)                     ; was running in
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)
    (op_YIELD)                          ; hand the machine to the other

    (op_LOCAL 1 0)
    (op_GLOBAL log)
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)
    (op_YIELD)

    (op_LOCAL 1 0)
    (op_GLOBAL log)
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)
    (op_RETURN 0))
  "(fn () (dotimes (i 3) (push tag log) (yield)))

Unrolled: a loop would need a counter, and the point here is the
interleaving, not the arithmetic.")

(defparameter +code-logger+
  (bard:assemble +lap-fn-logger+ :name "logger" :arity 0))

(defparameter +lap-fn-make-logger+
  `((op_CLOSE ,+code-logger+)           ; captures this frame, where tag lives
    (op_RETURN 1))
  "(fn (tag) (fn () ...))")

(defparameter +code-make-logger+
  (bard:assemble +lap-fn-make-logger+ :name "make-logger" :arity 1))

(defparameter +lap-two-threads+
  `((op_CONST a)
    (op_CLOSE ,+code-make-logger+)
    (op_CALL 1) (op_RECV 1)
    (op_GLOBAL _spawn)
    (op_CALL 1) (op_RECV 0)

    (op_CONST b)
    (op_CLOSE ,+code-make-logger+)
    (op_CALL 1) (op_RECV 1)
    (op_GLOBAL _spawn)
    (op_CALL 1) (op_RECV 0)

    (op_RETURN 0))                      ; this thread is done; the two
  "Two logger threads, spawned and left to alternate.

Each yields between entries, so the log interleaves. The tags come from
closures over separate make-logger frames, so the threads share the log
and nothing else.")

;;; ---------------------------------------------------------------------
;;; assembly -- the dynamic environment
;;; ---------------------------------------------------------------------

;;; A binding is an ordinary value, so a program names one with op_CONST
;;; and hands it to a primitive. op_GLOBAL takes a binding operand
;;; instead, which the assembler resolves for it.

(defparameter +lap-fn-read-out+
  '((op_GLOBAL *out*)
    (op_RETURN 1))
  "(fn () *out*) -- a callee, to show a rebinding reaches one.")

(defparameter +code-read-out+
  (bard:assemble +lap-fn-read-out+ :name "read-out" :arity 0))

(defparameter +lap-dynamic-extent+
  `((op_CONST ,(bard:global-binding '*out*))
    (op_CONST rebound)
    (op_GLOBAL _push-rebinding!)
    (op_CALL 2) (op_RECV 0)

    (op_CLOSE ,+code-read-out+)         ; the callee sees the rebinding
    (op_CALL 0) (op_RECV 1)

    (op_GLOBAL _pop-rebinding!)         ; and stops seeing it afterwards
    (op_CALL 0) (op_RECV 0)

    (op_CLOSE ,+code-read-out+)
    (op_CALL 0) (op_RECV 1)
    (op_RETURN 2))
  "*out* read inside a rebinding and again after popping it.")

(defparameter +lap-opt-in+
  `((op_CONST ,(bard:global-binding 'plain))
    (op_CONST ignored)
    (op_GLOBAL _push-rebinding!)        ; plain was never declared dynamic,
    (op_CALL 2) (op_RECV 0)             ; so op_GLOBAL never looks here
    (op_GLOBAL plain)
    (op_RETURN 1))
  "A rebinding pushed for an undeclared binding is not consulted.")

(defparameter +lap-fn-rebinder+
  `((op_CONST ,(bard:global-binding '*out*))
    (op_CONST a)
    (op_GLOBAL _push-rebinding!)
    (op_CALL 2) (op_RECV 0)
    (op_YIELD)                          ; the other thread runs here

    (op_GLOBAL *out*)                   ; still a, in this thread
    (op_GLOBAL log)
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)

    (op_GLOBAL _pop-rebinding!)
    (op_CALL 0) (op_RECV 0)
    (op_RETURN 0))
  "Rebinds *out*, yields, then logs what it still sees.")

(defparameter +code-rebinder+
  (bard:assemble +lap-fn-rebinder+ :name "rebinder" :arity 0))

(defparameter +lap-fn-reader+
  '((op_GLOBAL *out*)                   ; the other thread's rebinding is
    (op_GLOBAL log)                     ; not visible here
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)
    (op_YIELD)
    (op_RETURN 0))
  "Logs what *out* looks like from a thread that rebound nothing.")

(defparameter +code-reader+
  (bard:assemble +lap-fn-reader+ :name "reader" :arity 0))

(defparameter +lap-dynenv-is-per-thread+
  `((op_CLOSE ,+code-rebinder+)
    (op_GLOBAL _spawn) (op_CALL 1) (op_RECV 0)
    (op_CLOSE ,+code-reader+)
    (op_GLOBAL _spawn) (op_CALL 1) (op_RECV 0)
    (op_RETURN 0))
  "One thread rebinds *out*; the other must not see it.")

;;; ---------------------------------------------------------------------
;;; assembly -- failure
;;; ---------------------------------------------------------------------

(defparameter +lap-fn-faulter+
  '((op_GLOBAL also-undefined)          ; faults; a handler abandons this
    (op_RETURN 1))                      ; thread
  "A thread that cannot finish.")

(defparameter +code-faulter+
  (bard:assemble +lap-fn-faulter+ :name "faulter" :arity 0))

(defparameter +lap-fn-survivor+
  '((op_CONST survivor)
    (op_GLOBAL log)
    (op_GLOBAL _cons)
    (op_CALL 2) (op_RECV 1)
    (op_SET-GLOBAL log) (op_DROP)
    (op_RETURN 0))
  "A thread that finishes normally after the other one dies.")

(defparameter +code-survivor+
  (bard:assemble +lap-fn-survivor+ :name "survivor" :arity 0))

(defparameter +lap-faulting-thread+
  `((op_CLOSE ,+code-faulter+)
    (op_GLOBAL _spawn) (op_CALL 1) (op_RECV 0)
    (op_CLOSE ,+code-survivor+)
    (op_GLOBAL _spawn) (op_CALL 1) (op_RECV 0)
    (op_RETURN 0))
  "One thread faults and is abandoned; the other runs to completion.")

;;; ---------------------------------------------------------------------
;;; assembly -- the disassembler, and pending stages
;;; ---------------------------------------------------------------------

(defparameter +lap-operand-kinds-listing+
  '((op_CONST 42)                          ; :const   -- renders the value
    (op_GLOBAL +)                          ; :binding -- renders the name
    (op_CALL 2)                            ; :count
    (op_RECV 1)
    (op_GOTO done)                         ; :label   -- renders the index
  done
    (op_RETURN 1))
  "One instruction of each operand kind, for the disassembler.")

;;; ---------------------------------------------------------------------
;;; stage 2 -- the loop, op_CONST, op_RETURN
;;; ---------------------------------------------------------------------

(test |2.1 a constant|
  "op_CONST, op_RETURN, and thread termination."
  (is (equal '(42) (run-program +lap-const+)))
  (is (equal '() (run-program +lap-return-none+)))
  (is (equal '(1 2 3) (run-program +lap-return-three+))))

#+repl (run! '|2.1 a constant|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 3 -- bindings, primitive calls, receivers
;;; ---------------------------------------------------------------------

(test |2.2 calling a primitive|
  "op_GLOBAL, op_CALL's descriptor branch, and a receiver."
  (is (= 5 (run-1 +lap-call-primitive+)))
  (is (= 6 (run-1 +lap-call-primitive-direct+))))

#+repl (run! '|2.2 calling a primitive|)  ; => T

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

#+repl (run! '|redefinition reaches compiled code|)  ; => T

(test |receivers reconcile producer and consumer|
  "One call, four receivers."
  (is (= 5 (run-1 +lap-recv-one+)))
  (is (equal '() (run-program +lap-recv-none+)))
  (is (equal '(5 nil) (run-program +lap-recv-two-padded+)))
  (is (equal '((5)) (run-program +lap-recv-all+))))

#+repl (run! '|receivers reconcile producer and consumer|)  ; => T

(test |2.4 sequencing and definition|
  "op_SET-GLOBAL and op_DROP. Defining something is an ordinary instruction,
not a special mode."
  (is (= 30 (run-1 +lap-set-global-sequence+))))

#+repl (run! '|2.4 sequencing and definition|)  ; => T

(test |an unbound global is an error carrying its faulting pc|
  (let ((signalled nil))
    (handler-case (run-program +lap-global-unbound-error+)
      (bard:bard-error (e)
        (setf signalled t)
        (is (= 1 (bard:bard-error-pc e)))
        (is (not (null (bard:bard-error-frame e))))))
    (is (eq t signalled))))

#+repl (run! '|an unbound global is an error carrying its faulting pc|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 4 -- control
;;; ---------------------------------------------------------------------

(test |2.3 a conditional|
  "op_GOTO and op_BRANCH-FALSE, with labels resolved by the assembler."
  (flet ((classify (n) (set-global 'n n) (run-1 +lap-branch-false-both-arms+)))
    (is (string= "small" (classify 1)))
    (is (string= "big" (classify 7)))
    (is (string= "big" (classify 3)))))

#+repl (run! '|2.3 a conditional|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 5 -- frames and calls
;;; ---------------------------------------------------------------------

(test |2.5 a function|
  "op_CLOSE, op_LOCAL, op_CALL's function branch, and op_RETURN into a real parent."
  (is (equal '(49) (run-program +lap-call-fn+))))

#+repl (run! '|2.5 a function|)  ; => T

(test |arguments arrive in order|
  "The first argument lands in slot 0. Verified with subtraction, since
squaring would be identical either way."
  (is (equal '(7) (run-program +lap-call-fn-arg-order+)))
  (is (equal '(-7) (run-program +lap-call-fn-arg-order-reversed+))))

#+repl (run! '|arguments arrive in order|)  ; => T

(test |calls nest|
  "Each call links a parent; each return restores it."
  (is (equal '(49) (run-program +lap-call-fn-nested+))))

#+repl (run! '|calls nest|)  ; => T

(test |a bytecode function delivers multiple values|
  "Same callee, three receivers."
  (is (equal '(10 3) (run-program +lap-recv-two-from-fn+)))
  (is (equal '(10) (run-program +lap-recv-one-from-fn+)))
  (is (equal '((10 3)) (run-program +lap-recv-all-from-fn+))))

#+repl (run! '|a bytecode function delivers multiple values|)  ; => T

(test |arity is checked against the code object|
  "The error names the op_CALL that faulted, not the instruction after it --
the same P5 property as an unbound global."
  (let ((signalled nil))
    (handler-case (run-program +lap-call-fn-arity-error+)
      (bard:bard-error (e)
        (setf signalled t)
        (is (= 2 (bard:bard-error-pc e)))))
    (is (eq t signalled))))

#+repl (run! '|arity is checked against the code object|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 6 -- the lexical chain
;;; ---------------------------------------------------------------------

(test |2.6 a closure over a mutable variable|
  "Two counters made from two calls increment independently. Nothing in
the machine arranges that; the two frames were already separate
computations."
  (is (equal '(11 12 101 13) (run-program +lap-counters-independent+))))

#+repl (run! '|2.6 a closure over a mutable variable|)  ; => T
#+repl (run-program +lap-counters-independent+)         ; => (11 12 101 13)

(test |op_SET-LOCAL writes the slot and leaves its value|
  "Reading the local back afterwards shows the write landed."
  (is (equal '(8)
             (run-program `((op_CONST 7) (op_CLOSE ,+code-set-local+) (op_CALL 1) (op_RECV 1) (op_RETURN 1))))))

#+repl (run! '|op_SET-LOCAL writes the slot and leaves its value|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 7 -- tail calls
;;; ---------------------------------------------------------------------

(test |2.7 a tail call does not return to its caller|
  "The caller's frame is abandoned, so the callee returns past it."
  (is (equal '(49) (run-program +lap-tailcall+)))
  (is (equal '(5) (run-program +lap-tailcall-primitive+))))

#+repl (run! '|2.7 a tail call does not return to its caller|)  ; => T

(test |tail recursion runs in constant space|
  "A hundred thousand deep. If op_TAILCALL linked a parent, this would build
a chain a hundred thousand frames long."
  (is (equal '(done) (run-program +lap-tailcall-deep+))))

#+repl (run! '|tail recursion runs in constant space|)  ; => T
#+repl (run-program +lap-tailcall-deep+)                ; => (DONE)

;;; ---------------------------------------------------------------------
;;; stage 9 -- threads
;;; ---------------------------------------------------------------------

(test |2.9 two threads alternate|
  "op_YIELD hands the machine to the next thread in rotation. A thread is
a frame you kept, so the switch is an assignment."
  (set-global 'log nil)
  (run-program +lap-two-threads+)
  (is (equal '(b a b a b a)
             (bard:binding-value (bard:global-binding 'log)))))

#+repl (run! '|2.9 two threads alternate|)  ; => T

(test |a lone thread yields to itself|
  "op_YIELD with nothing else runnable is a no-op, so a program need not
know how many threads exist."
  (is (equal '(42) (run-program '((op_YIELD) (op_CONST 42) (op_YIELD) (op_RETURN 1))))))

#+repl (run! '|a lone thread yields to itself|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 10 -- the dynamic environment
;;; ---------------------------------------------------------------------

(defun declare-dynamic (name)
  (setf (bard:binding-dynamic? (bard:global-binding name)) t))

(test |2.10 a rebinding reaches a callee and ends when popped|
  (declare-dynamic '*out*)
  (set-global '*out* 'base)
  (is (equal '(rebound base) (run-program +lap-dynamic-extent+))))

#+repl (run! '|2.10 a rebinding reaches a callee and ends when popped|)  ; => T

(test |dynamic variables are opt-in|
  "An undeclared binding never consults the dynamic environment, so it
costs one test and nothing else."
  (set-global 'plain 'cell)
  (is (eq 'cell (run-1 +lap-opt-in+))))

#+repl (run! '|dynamic variables are opt-in|)  ; => T

(test |the dynamic environment is per thread|
  "A rebinding in one thread is invisible in another, which is what makes
it per task rather than global."
  (declare-dynamic '*out*)
  (set-global '*out* 'base)
  (set-global 'log nil)
  (run-program +lap-dynenv-is-per-thread+)
  (is (equal '(a base) (bard:binding-value (bard:global-binding 'log)))))

#+repl (run! '|the dynamic environment is per thread|)  ; => T

;;; ---------------------------------------------------------------------
;;; stage 11 -- failure and resumption
;;; ---------------------------------------------------------------------

(test |2.11 define the missing function and resume|
  "The essay's headline, in five instructions. bar has no value; the
handler runs inside the environment where that was discovered, defines
bar, and retries the instruction that faulted. The 7 is still on the
operand stack, so the call then succeeds."
  (let ((binding (bard:global-binding 'bar)))
    (setf (bard:binding-value binding) nil
          (bard:binding-bound? binding) nil)
    (is (equal '(49)
               (handler-bind
                   ((bard:bard-error
                      (lambda (c)
                        (declare (ignore c))
                        (setf (bard:binding-value binding)
                              (bard:make-fn +code-square+)
                              (bard:binding-bound? binding) t)
                        (invoke-restart 'bard:retry))))
                 (run-program +lap-global-unbound-error+))))))

#+repl (run! '|2.11 define the missing function and resume|)  ; => T

(test |a handler may supply a value instead|
  "The failed operation is replaced rather than retried, and stepping
resumes at the next instruction."
  (let ((binding (bard:global-binding 'never-defined)))
    (setf (bard:binding-value binding) nil
          (bard:binding-bound? binding) nil)
    (is (equal '(99)
               (handler-bind
                   ((bard:bard-error
                      (lambda (c)
                        (declare (ignore c))
                        (invoke-restart 'bard:supply-value 99))))
                 (run-program '((op_GLOBAL never-defined) (op_RETURN 1))))))))

#+repl (run! '|a handler may supply a value instead|)  ; => T

(test |a handler may abandon the thread|
  "The other threads carry on."
  (set-global 'log nil)
  (let ((binding (bard:global-binding 'also-undefined)))
    (setf (bard:binding-value binding) nil
          (bard:binding-bound? binding) nil))
  (handler-bind ((bard:bard-error
                   (lambda (c)
                     (declare (ignore c))
                     (invoke-restart 'bard:abort-thread))))
    (run-program +lap-faulting-thread+))
  (is (equal '(survivor) (bard:binding-value (bard:global-binding 'log)))))

#+repl (run! '|a handler may abandon the thread|)  ; => T

(test |the frame is intact when the handler runs|
  "Not unwound first: the operand the faulting instruction was about to
use is still there, which is what makes repair possible at all."
  (let ((binding (bard:global-binding 'bar))
        (operands nil))
    (setf (bard:binding-value binding) nil
          (bard:binding-bound? binding) nil)
    (handler-bind ((bard:bard-error
                     (lambda (c)
                       (setf operands (bard::frame-operands (bard:bard-error-frame c)))
                       (invoke-restart 'bard:supply-value
                                       (bard:make-fn +code-square+)))))
      (run-program +lap-global-unbound-error+))
    (is (equal '(7) operands))))

#+repl (run! '|the frame is intact when the handler runs|)  ; => T

;;; ---------------------------------------------------------------------
;;; the compiler
;;; ---------------------------------------------------------------------
;;; Source forms are short enough to read inside a test, so unlike
;;; assembly they are not lifted to toplevel.

(test |the compiler handles each special form|
  (is (equal '(42) (bard:eval-form 42)))
  (is (equal '(hello) (bard:eval-form ''hello)))
  (is (equal '(5) (bard:eval-form '(+ 2 3))))
  (is (equal '(small) (bard:eval-form '(if (< 1 2) 'small 'big))))
  (is (equal '(big) (bard:eval-form '(if (< 5 2) 'small 'big))))
  (is (equal '(30) (bard:eval-form '(begin (set! x 10) (set! y 20) (+ x y)))))
  (is (equal '(49) (bard:eval-form '((method (n) (* n n)) 7)))))

#+repl (run! '|the compiler handles each special form|)  ; => T

(test |arguments and lexical scope|
  "The first argument lands in slot 0, and an inner method reaches the
enclosing frame."
  (is (equal '(7) (bard:eval-form '((method (a b) (- a b)) 10 3))))
  (is (equal '(-7) (bard:eval-form '((method (a b) (- a b)) 3 10))))
  (is (equal '(5) (bard:eval-form '((method (n) ((method () n))) 5)))))

#+repl (run! '|arguments and lexical scope|)  ; => T

(test |the three contexts give the multiple-value semantics|
  "Tail propagates, value truncates, effect discards -- with no rule
beyond the contexts themselves."
  (is (equal '(1 2 3) (bard:eval-form '(values 1 2 3))))
  (is (equal '(11) (bard:eval-form '(+ (values 1 2 3) 10))))
  (is (equal '(1 2) (bard:eval-form '(if (< 1 2) (values 1 2) (values 3 4)))))
  (is (equal '(3 4) (bard:eval-form '(if (< 5 2) (values 1 2) (values 3 4)))))
  (is (equal '(9) (bard:eval-form '(begin (values 1 2) 9)))))

#+repl (run! '|the three contexts give the multiple-value semantics|)  ; => T

(test |a call in tail position becomes a tail call|
  "Not an optimization: op_TAILCALL is semantics, so the analysis has to
happen at compile time."
  (let ((inner (with-output-to-string (s)
                 (bard:disassemble
                  (bard:compile-method '(n) '((f n)) '() :name "tail") :stream s))))
    (is (search "op_TAILCALL" inner))
    (is (not (search "op_RECV" inner)))))

#+repl (run! '|a call in tail position becomes a tail call|)  ; => T

(test |frame size is computed rather than guessed|
  "The assembler's generous default is gone once a compiler is present."
  (let ((code (bard:compile-form '(+ 1 (+ 2 3)))))
    (is (< (bard:code-frame-size code) 20))))

#+repl (run! '|frame size is computed rather than guessed|)  ; => T
#+repl (bard:disassemble (bard:compile-form '(if (< 1 2) 'small 'big) :name "classify"))

;;; ---------------------------------------------------------------------
;;; the kernel language, tested in itself
;;; ---------------------------------------------------------------------

(test |the bard test suite passes|
  "test/kernel-tests.bard is written in Bard and checks its own results.
It reports a failure count, which must be zero. If this fails, run the
file directly to see which check reported FAIL."
  (bard:load-bard-file
   (asdf:system-relative-pathname :bard "test/kernel-tests.bard"))
  (is (eql 0 (bard:binding-value (bard:global-binding 'failures)))))

#+repl (run! '|the bard test suite passes|)  ; => T
#+repl (bard:load-bard-file
        (asdf:system-relative-pathname :bard "test/kernel-tests.bard"))

;;; ---------------------------------------------------------------------
;;; the prelude
;;; ---------------------------------------------------------------------

(test |the prelude loads and its macros work|
  "lib/prelude.bard is Bard defining Bard: define, let, when, unless,
and, or, cond, and some list handling, all in six special forms plus
defmacro."
  (bard:load-bard-file (asdf:system-relative-pathname :bard "lib/prelude.bard"))
  (is (equal '(3) (bard:eval-form '(let ((a 1) (b 2)) (+ a b)))))
  (is (equal '(yes) (bard:eval-form '(when (< 1 2) 'yes))))
  (is (equal '(3) (bard:eval-form '(and 1 2 3))))
  (is (equal '(7) (bard:eval-form '(or nothing nothing 7))))
  (is (equal '(medium)
             (bard:eval-form '(cond ((< 5 2) 'small) ((< 1 2) 'medium) (else 'big)))))
  (is (equal '((3 2 1)) (bard:eval-form '(reverse (list 1 2 3)))))
  (is (equal '(3628800)
             (bard:eval-form '(begin (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
                                     (fact 10))))))

#+repl (run! '|the prelude loads and its macros work|)  ; => T

(test |a variadic method collects its extra arguments|
  "& marks the rest parameter. This is checked separately because the
macro-application path places arguments itself and once got it wrong."
  (is (equal '((1 (2 3))) (bard:eval-form '((method (a & more) (list a more)) 1 2 3))))
  (is (equal '((1 nil)) (bard:eval-form '((method (a & more) (list a more)) 1))))
  (is (equal '((1 2 3)) (bard:eval-form '((method (& all) all) 1 2 3)))))

#+repl (run! '|a variadic method collects its extra arguments|)  ; => T

;;; ---------------------------------------------------------------------
;;; the assembler and disassembler
;;; ---------------------------------------------------------------------

(test |disassembly names every opcode and renders every operand|
  "You should never have to look up what a number means."
  (let ((text (with-output-to-string (s)
                (bard:disassemble (bard:assemble +lap-operand-kinds-listing+ :name "sample")
                                  :stream s))))
    (dolist (want '("op_CONST 42" "op_GLOBAL +" "op_CALL 2" "op_RECV 1"
                    "op_GOTO 5" "op_RETURN 1" "sample"))
      (is (search want text)))))

#+repl (run! '|disassembly names every opcode and renders every operand|)  ; => T
#+repl (bard:disassemble (bard:assemble +lap-operand-kinds-listing+ :name "sample"))
       ; prints the listing

(test |the assembler rejects malformed input|
  (signals error (bard:assemble '((op_CONST))))
  (signals error (bard:assemble '((op_CONST 1 2))))
  (signals error (bard:assemble '((op_GOTO nowhere))))
  (signals error (bard:assemble '((op_RETURN 0)) :arity 2 :n-locals 1)))

#+repl (run! '|the assembler rejects malformed input|)  ; => T

;;; ---------------------------------------------------------------------
;;; the instruction set is complete
;;; ---------------------------------------------------------------------

(test |every instruction is implemented|
  "Fifteen, and nothing left pending at the machine level. Stage 10
changes what op_GLOBAL does; stage 11 changes how failure is reported.
Neither adds an instruction."
  (is (= 15 (length bard::*opcode-specs*))))

#+repl (run! '|every instruction is implemented|)  ; => T
