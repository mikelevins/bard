;;;; ***********************************************************************
;;;;
;;;; Name:          vm.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the machine
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; The machine steps a reified computation. A frame is that
;;; computation. See doc/kernel.md.
;;;
;;; Stages implemented so far (doc/kernel-tutorial.md part 3):
;;;
;;;   1  representations                                    done
;;;   2  the loop, op_CONST, op_RETURN                            done
;;;   3  bindings, primitive calls, receivers               done
;;;   4  control                                            done
;;;   5  frames and calls                                   done
;;;   6  the lexical chain                                  done
;;;   7  tail calls                                         pending
;;;   8  multiple values (op_RETURN n, op_RECV k, op_RECV-ALL)       done
;;;   9  threads                                            pending
;;;  10  the dynamic environment                            pending
;;;  11  the error hook                                     pending
;;;
;;; Unimplemented instructions signal rather than misbehave, so the
;;; ladder is visible from a backtrace.

;;; ---------------------------------------------------------------------
;;; errors
;;; ---------------------------------------------------------------------
;;; Stage 11 replaces this with a hook that is called before anything
;;; unwinds, and that can resume at the faulting instruction. Until
;;; then it carries the two things that hook will need: the frame, and
;;; the pc of the instruction that faulted -- not the one after it.

(define-condition bard-error (simple-error)
  ((frame :initarg :frame :reader bard-error-frame :initform nil)
   (pc :initarg :pc :reader bard-error-pc :initform nil))
  (:report (lambda (c stream)
             (format stream "~?~@[~&  at ~A~]"
                     (simple-condition-format-control c)
                     (simple-condition-format-arguments c)
                     (let ((frame (bard-error-frame c))
                           (pc (bard-error-pc c)))
                       (when (and frame pc)
                         (let ((code (fn-code (frame-fn frame))))
                           (format nil "~A instruction ~D: ~A"
                                   (or (code-name code) "anonymous")
                                   pc
                                   (instruction-string code pc)))))))))

(defun bard-error (frame pc format &rest args)
  (error 'bard-error :frame frame :pc pc
                     :format-control format :format-arguments args))

;;; ---------------------------------------------------------------------
;;; tracing
;;; ---------------------------------------------------------------------

(defparameter *trace* nil
  "When true, print each instruction as it executes, with its operands
rendered and the operand stack shown.")

(defun trace-instruction (frame pc)
  (let ((code (fn-code (frame-fn frame))))
    (format *trace-output* "~&~4D: ~28A ~A~%"
            pc
            (instruction-string code pc)
            (frame-operands frame))))

;;; ---------------------------------------------------------------------
;;; calling
;;; ---------------------------------------------------------------------
;;; op_CALL asks the callee's descriptor for a handler rather than assuming
;;; a bytecode function. That one indirection is what will later let
;;; generic functions, foreign functions, and native-compiled functions
;;; all be reached by this instruction.
;;;
;;; THE HANDLER CONTRACT
;;;
;;; A handler takes (callee n-args frame pc) and returns the frame the
;;; machine should continue stepping. That return value carries more
;;; information than it looks like it does, and both op_CALL and
;;; op_TAILCALL depend on it:
;;;
;;;   a NEW frame     the callee is a computation of its own. It will
;;;                   run, and eventually op_RETURN into whatever frame
;;;                   it was given as its parent. Its values arrive
;;;                   there later.
;;;
;;;   the SAME frame  the callee ran to completion in place. It has
;;;                   already pushed its values and their count onto
;;;                   this frame's operand area. Nothing arrives later.
;;;
;;; Nothing else distinguishes the two cases, deliberately. op_TAILCALL
;;; has to tell them apart: it abandons the caller's frame, so a callee
;;; that would have returned there must be re-pointed at the caller's
;;; parent, while a callee that already delivered its values in place
;;; needs those values forwarded instead.
;;;
;;; op_TAILCALL asks the returned frame rather than testing the callee's
;;; type. That is why a new kind of applicable -- a generic function, a
;;; foreign function, native code -- behaves correctly by honouring this
;;; contract rather than by being added to a list of special cases. It is
;;; what property P3 buys, and it is only bought for as long as new
;;; handlers keep to it.
;;;
;;; PC is the faulting instruction -- the op_CALL itself, not the one
;;; after it -- because a handler that signals must report where the
;;; fault was, per P5.

(defun call-primitive (callee n-args frame pc)
  (let ((arity (primitive-arity callee)))
    (unless (= n-args arity)
      (bard-error frame pc
                  "~A takes ~D argument~:P, called with ~D."
                  (primitive-name callee) arity n-args))
    (let ((args (make-list n-args)))
      (loop for i from (1- n-args) downto 0
            do (setf (nth i args) (frame-pop frame)))
      ;; Primitives deliver a count like everything else. That
      ;; uniformity is what lets a receiver follow any call without
      ;; knowing what it called.
      (frame-push frame (apply (primitive-function callee) args))
      (frame-push frame 1)))
  frame)

(defun call-fn (callee n-args frame pc)
  "Allocating the frame is what calling means."
  (let* ((code (fn-code callee))
         (arity (code-arity code)))
    (unless (= n-args arity)
      (bard-error frame pc
                  "~A takes ~D argument~:P, called with ~D."
                  (or (code-name code) "anonymous") arity n-args))
    (let* ((new (make-frame callee :parent frame))
           (slots (frame-slots new)))
      ;; Arguments were pushed left to right, so popping delivers them
      ;; last first. Filling downward puts the first argument in slot 0.
      (loop for i from (1- n-args) downto 0
            do (setf (svref slots i) (frame-pop frame)))
      new)))

(setf (descriptor-call-handler *primitive-descriptor*) #'call-primitive
      (descriptor-call-handler *fn-descriptor*) #'call-fn)

;;; ---------------------------------------------------------------------
;;; the lexical chain
;;; ---------------------------------------------------------------------
;;; A closure captures the frame it was created in, so environments and
;;; frames are one representation and the lexical chain is a walk along
;;; CAPTURED-FRAME links. UP counts those links: 0 is the current frame,
;;; 1 the frame its function was created in, and so on.

(declaim (inline lexical-frame))

(defun lexical-frame (frame up pc)
  "The frame UP levels outward along the lexical chain."
  (let ((f frame))
    (dotimes (i up f)
      (let ((next (fn-captured-frame (frame-fn f))))
        (unless next
          (bard-error frame pc "Lexical level ~D reaches past the chain." up))
        (setf f next)))))

;;; ---------------------------------------------------------------------
;;; delivering values
;;; ---------------------------------------------------------------------
;;; Both op_RETURN and a tail call to a primitive have to hand values, with
;;; their count, from one frame to its parent. Factored out so the two
;;; agree by construction.

(defun pop-values (frame n)
  "Pop the top N values of FRAME and return them as a list, bottom first."
  (let ((values '()))
    (dotimes (i n values)
      (push (frame-pop frame) values))))

(defun deliver-values (frame n)
  "Move the top N values of FRAME into its parent, followed by the count,
and return the parent."
  (let ((values (pop-values frame n))
        (parent (frame-parent frame)))
    (dolist (v values) (frame-push parent v))
    (frame-push parent n)
    parent))

;;; ---------------------------------------------------------------------
;;; receivers
;;; ---------------------------------------------------------------------

(defun receive (frame k)
  "Adjust a delivered return to exactly K values."
  (let ((count (frame-pop frame)))
    (cond ((> count k) (dotimes (i (- count k)) (frame-pop frame)))
          ((< count k) (dotimes (i (- k count)) (frame-push frame *nothing*))))))

(defun receive-all (frame)
  "Collect a delivered return into one list."
  (let ((count (frame-pop frame))
        (values '()))
    (dotimes (i count) (push (frame-pop frame) values))
    (frame-push frame values)))

;;; ---------------------------------------------------------------------
;;; the loop
;;; ---------------------------------------------------------------------

(defun run (frame)
  "Step FRAME until a op_RETURN into a nil parent ends the computation.
Returns the delivered values as a list."
  (loop
    (let* ((code (fn-code (frame-fn frame)))
           (ins (code-instructions code))
           (pc (frame-pc frame))
           (base (* 3 pc))
           (op (aref ins base))
           (a (aref ins (+ base 1)))
           (b (aref ins (+ base 2))))
      ;; PC advances before the instruction runs, so PC above is the
      ;; faulting instruction and is what the error hook will need.
      (when *trace* (trace-instruction frame pc))
      (setf (frame-pc frame) (1+ pc))
      (dispatch-on-opcode op
        ;; ----- values -----
        (op_CONST
         (frame-push frame (svref (code-constants code) a)))

        (op_LOCAL
         (frame-push frame (svref (frame-slots (lexical-frame frame a pc)) b)))

        (op_CLOSE
         (frame-push frame (make-fn (svref (code-constants code) a) frame)))

        (op_GLOBAL
         (let ((binding (svref (code-constants code) a)))
           (unless (binding-bound? binding)
             (bard-error frame pc "~A is unbound." (binding-name binding)))
           (frame-push frame (binding-value binding))))

        ;; ----- stores -----
        (op_SET-LOCAL
         (setf (svref (frame-slots (lexical-frame frame a pc)) b)
               (frame-top frame)))      ; does not pop

        (op_SET-GLOBAL
         (let ((binding (svref (code-constants code) a)))
           (setf (binding-value binding) (frame-top frame)
                 (binding-bound? binding) t)))

        (op_DROP
         (frame-pop frame))

        ;; ----- control -----
        (op_GOTO
         (setf (frame-pc frame) a))

        (op_BRANCH-FALSE
         (when (null (frame-pop frame))
           (setf (frame-pc frame) a)))

        ;; ----- calling -----
        (op_CALL
         (let* ((callee (frame-pop frame))
                (handler (descriptor-call-handler (descriptor-of callee))))
           (unless handler
             (bard-error frame pc "~S is not applicable." callee))
           (setf frame (funcall handler callee a frame pc))))

        (op_RECV
         (receive frame a))

        (op_RECV-ALL
         (receive-all frame))

        (op_RETURN
         (if (frame-parent frame)
             (setf frame (deliver-values frame a))
             (return (pop-values frame a))))

        (op_TAILCALL
         ;; The callee is reached exactly as op_CALL reaches it. What differs
         ;; is what becomes of this frame afterwards: it is abandoned, so
         ;; the callee returns to our parent rather than to us.
         (let* ((callee (frame-pop frame))
                (handler (descriptor-call-handler (descriptor-of callee)))
                (caller frame))
           (unless handler
             (bard-error frame pc "~S is not applicable." callee))
           (let ((next (funcall handler callee a caller pc)))
             (cond ((eq next caller)
                    ;; It ran in place -- a primitive. Its values are here
                    ;; already, so forward them where our return would go.
                    (let ((count (frame-pop caller)))
                      (if (frame-parent caller)
                          (setf frame (deliver-values caller count))
                          (return (pop-values caller count)))))
                   (t
                    ;; A fresh frame. Inheriting our parent is what makes
                    ;; the call a tail call.
                    (setf (frame-parent next) (frame-parent caller)
                          frame next))))))

        ;; ----- not yet implemented -----
        (t
         (bard-error frame pc "~A is not implemented yet." (opcode-name op)))))))

(defun run-code (code)
  "Run CODE as a whole computation and return its values as a list."
  (run (make-frame (make-fn code) :parent nil)))

#+repl (run-code (assemble '((op_CONST 42) (op_RETURN 1)) :name "answer")) ; => (42)
#+repl (let ((*trace* t)) (run-code (assemble '((op_CONST 42) (op_RETURN 1)))))
       ; => (42), with each instruction traced
