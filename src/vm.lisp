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
;;;    1  representations                done
;;;    2  the loop, op_CONST, op_RETURN  done
;;;    3  bindings, calls, receivers     done
;;;    4  control                        done
;;;    5  frames and calls               done
;;;    6  the lexical chain              done
;;;    7  tail calls                     done
;;;    8  multiple values                done
;;;    9  threads                        done
;;;   10  the dynamic environment        done
;;;   11  the error hook                 done
;;;
;;; The ladder is complete: fifteen instructions, one register, seven
;;; representations. What is missing above it -- an object system,
;;; conditions, a debugger, modules, a compiler -- is not yet written
;;; rather than precluded.

;;; ---------------------------------------------------------------------
;;; errors
;;; ---------------------------------------------------------------------
;;; Failure is reported without unwinding first: the condition is
;;; signalled with the frame and the faulting pc in hand, so a handler
;;; runs inside the environment where the fault happened and can repair
;;; it. Three restarts say what to do next. See "Failure and resumption"
;;; in doc/kernel-tutorial.md.
;;;
;;; The restarts are established only when a fault occurs, so the
;;; ordinary path pays nothing for them.

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
  (restart-case
      (error 'bard-error :frame frame :pc pc
                         :format-control format :format-arguments args)
    (retry ()
      :report "Run the faulting instruction again."
      (setf (frame-pc frame) pc)
      (throw 'resume nil))
    (supply-value (value)
      :report "Use a value in place of the failed operation."
      :interactive (lambda () (list (eval (read))))
      (frame-push frame value)
      (throw 'resume nil))
    (abort-thread ()
      :report "Abandon this thread."
      (throw 'resume :abort-thread))))

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
;;; A handler takes (callee n-args frame pc) and returns the frame the
;;; machine should continue stepping. Whether that is a new frame or the
;;; one it was given is the whole of the contract, and op_TAILCALL keys
;;; on it. See "The handler contract" in doc/kernel-tutorial.md.
;;;
;;; PC is the faulting instruction, not the one after it, so a handler
;;; that signals reports where the fault was.

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
;;; reading and assigning globals
;;; ---------------------------------------------------------------------
;;; Dynamic variables are opt-in: only a binding declared dynamic
;;; consults the current thread's rebindings, and an ordinary one reads
;;; and assigns its cell directly. The flag is false for nearly every
;;; binding, so the ordinary path costs one test on an object already in
;;; hand.
;;;
;;; Rebindings are per thread over a shared cell, and a spawned thread
;;; starts with none.

(defun binding-ref (binding frame pc)
  "BINDING's value: its innermost rebinding in this thread if it is
dynamic and has one, otherwise its cell."
  (let ((entry (and (binding-dynamic? binding)
                    (assoc binding (thread-dynenv *current-thread*)))))
    (cond (entry (cdr entry))
          ((binding-bound? binding) (binding-value binding))
          (t (bard-error frame pc "~A is unbound." (binding-name binding))))))

(defun binding-set! (binding value)
  "Assign BINDING: its innermost rebinding if it has one, otherwise its
cell."
  (let ((entry (and (binding-dynamic? binding)
                    (assoc binding (thread-dynenv *current-thread*)))))
    (if entry
        (setf (cdr entry) value)
        (setf (binding-value binding) value
              (binding-bound? binding) t))
    value))

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
;;; threads
;;; ---------------------------------------------------------------------
;;; A thread is a frame you kept. Switching is storing the current frame
;;; into one thread and loading another's; nothing is saved or restored,
;;; because there is nothing outside the frame to save.
;;;
;;; The rotation is round-robin over a list. Anything smarter belongs
;;; above the machine and will replace this.

(defvar *threads* '()
  "The threads the machine is rotating among.")

(defvar *current-thread* nil)

(defun next-runnable ()
  "The next thread in rotation that has not finished, or NIL if none has
work left."
  (let* ((all *threads*)
         (n (length all))
         (start (or (position *current-thread* all) 0)))
    (loop for i from 1 to n
          for thread = (nth (mod (+ start i) n) all)
          unless (eq (thread-status thread) :finished)
            do (return thread))))

(defun spawn (fn)
  "Make a thread over FN and add it to the rotation."
  (let ((thread (make-thread (make-frame fn :parent nil))))
    (setf *threads* (append *threads* (list thread)))
    thread))

(defun finish-current-thread (values)
  "Record VALUES as the current thread's result, and return the next
runnable thread -- or NIL if that was the last one."
  (setf (thread-result *current-thread*) values
        (thread-status *current-thread*) :finished)
  (next-runnable))

;;; ---------------------------------------------------------------------
;;; the loop
;;; ---------------------------------------------------------------------

(defun run (frame)
  "Run FRAME as a thread, rotating among any threads it spawns, until
none has work left. Returns FRAME's own delivered values."
  (let* ((thread (make-thread frame))
         (*threads* (list thread))
         (*current-thread* thread))
    (%run frame)
    (thread-result thread)))

(defun %run (frame)
  (loop
    ;; A fault throws here once a restart has said what to do. RETRY and
    ;; SUPPLY-VALUE have already adjusted the frame, so stepping just
    ;; continues from wherever they left it.
    (let ((signal
            (catch 'resume
              (let* ((code (fn-code (frame-fn frame)))
                     (ins (code-instructions code))
                     (pc (frame-pc frame))
                     (base (* 3 pc))
                     (op (aref ins base))
                     (a (aref ins (+ base 1)))
                     (b (aref ins (+ base 2))))
                ;; PC advances before the instruction runs, so PC above is
                ;; the faulting instruction, which is what a handler needs.
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
         (frame-push frame (binding-ref (svref (code-constants code) a) frame pc)))

        ;; ----- stores -----
        (op_SET-LOCAL
         (setf (svref (frame-slots (lexical-frame frame a pc)) b)
               (frame-top frame)))      ; does not pop

        (op_SET-GLOBAL
         (binding-set! (svref (code-constants code) a) (frame-top frame)))

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
         (cond ((frame-parent frame)
                (setf frame (deliver-values frame a)))
               (t
                ;; No parent: this thread is finished. Hand the machine
                ;; to whoever is left.
                (let ((next (finish-current-thread (pop-values frame a))))
                  (unless next (return))
                  (setf *current-thread* next
                        frame (thread-frame next))))))

        (op_YIELD
         (setf (thread-frame *current-thread*) frame)
         (let ((next (next-runnable)))
           (when next
             (setf *current-thread* next
                   frame (thread-frame next)))))

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
                          (let ((next (finish-current-thread
                                       (pop-values caller count))))
                            (unless next (return))
                            (setf *current-thread* next
                                  frame (thread-frame next))))))
                   (t
                    ;; A fresh frame. Inheriting our parent is what makes
                    ;; the call a tail call.
                    (setf (frame-parent next) (frame-parent caller)
                          frame next))))))

        ;; Every instruction is implemented, so reaching here means the
        ;; opcode word is not one -- corrupt code, or a code object built
        ;; by something that does not agree with this machine.
        (t
         (bard-error frame pc "~A is not an instruction." (opcode-name op)))))
              nil)))
      (when (eq signal :abort-thread)
        (let ((next (finish-current-thread '())))
          (unless next (return))
          (setf *current-thread* next
                frame (thread-frame next))))))
  (values))

(defun run-code (code)
  "Run CODE as a whole computation and return its values as a list."
  (run (make-frame (make-fn code) :parent nil)))

#+repl (run-code (assemble '((op_CONST 42) (op_RETURN 1)) :name "answer")) ; => (42)
#+repl (let ((*trace* t)) (run-code (assemble '((op_CONST 42) (op_RETURN 1)))))
       ; => (42), with each instruction traced
