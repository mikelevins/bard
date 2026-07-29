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
;;;   2  the loop, CONST, RETURN                            done
;;;   3  bindings, primitive calls, receivers               done
;;;   4  control                                            done
;;;   5  frames and calls                                   done
;;;   6  the lexical chain                                  pending
;;;   7  tail calls                                         pending
;;;   8  multiple values (RETURN n, RECV k, RECV-ALL)       done
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
;;; CALL asks the callee's descriptor for a handler rather than assuming
;;; a bytecode function. That one indirection is what will later let
;;; generic functions, foreign functions, and native-compiled functions
;;; all be reached by this instruction.
;;;
;;; A handler takes (callee n-args frame pc) and returns the frame to
;;; continue in. A primitive stays in the caller's frame; a bytecode
;;; function returns a fresh one. PC is the faulting instruction -- the
;;; CALL itself, not the instruction after it -- because a handler that
;;; signals must report where the fault was, per P5.

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
  "Step FRAME until a RETURN into a nil parent ends the computation.
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
      (case op
        ;; ----- values -----
        (#.+op-const+
         (frame-push frame (svref (code-constants code) a)))

        (#.+op-local+
         (unless (zerop a)
           (bard-error frame pc "The lexical chain is not implemented yet."))
         (frame-push frame (svref (frame-slots frame) b)))

        (#.+op-close+
         (frame-push frame (make-fn (svref (code-constants code) a) frame)))

        (#.+op-global+
         (let ((binding (svref (code-constants code) a)))
           (unless (binding-bound? binding)
             (bard-error frame pc "~A is unbound." (binding-name binding)))
           (frame-push frame (binding-value binding))))

        ;; ----- stores -----
        (#.+op-set-global+
         (let ((binding (svref (code-constants code) a)))
           (setf (binding-value binding) (frame-top frame)
                 (binding-bound? binding) t)))

        (#.+op-drop+
         (frame-pop frame))

        ;; ----- control -----
        (#.+op-goto+
         (setf (frame-pc frame) a))

        (#.+op-branch-false+
         (when (null (frame-pop frame))
           (setf (frame-pc frame) a)))

        ;; ----- calling -----
        (#.+op-call+
         (let* ((callee (frame-pop frame))
                (handler (descriptor-call-handler (descriptor-of callee))))
           (unless handler
             (bard-error frame pc "~S is not applicable." callee))
           (setf frame (funcall handler callee a frame pc))))

        (#.+op-recv+
         (receive frame a))

        (#.+op-recv-all+
         (receive-all frame))

        (#.+op-return+
         (let ((values '())
               (parent (frame-parent frame)))
           (dotimes (i a) (push (frame-pop frame) values))
           (cond ((null parent)
                  (return values))
                 (t
                  (dolist (v values) (frame-push parent v))
                  (frame-push parent a)
                  (setf frame parent)))))

        ;; ----- not yet implemented -----
        (t
         (bard-error frame pc "~A is not implemented yet." (opcode-name op)))))))

(defun run-code (code)
  "Run CODE as a whole computation and return its values as a list."
  (run (make-frame (make-fn code) :parent nil)))

#+repl (run-code (assemble '((const 42) (return 1)) :name "answer"))
#+repl (let ((*trace* t)) (run-code (assemble '((const 42) (return 1)))))
