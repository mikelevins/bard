;;;; ***********************************************************************
;;;;
;;;; Name:          types.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the seven representations of the machine
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; The machine has seven representations: frame, function, code,
;;; binding, descriptor, primitive, and thread. See doc/kernel.md §2.
;;;
;;; A frame is a computation. Everything else exists to be reached from
;;; one.

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------
;;; NOTHING is false, the empty list, and the empty collection. For the
;;; bootstrap it is CL's NIL, which already plays all three roles.

(defparameter *nothing* nil
  "The value that is false, the empty list, and the empty collection.")

;;; ---------------------------------------------------------------------
;;; descriptors
;;; ---------------------------------------------------------------------
;;; Every value carries a descriptor, and the machine never interprets
;;; one. It stores, copies, and compares them, and asks one for its
;;; CALL-HANDLER when op_CALL needs to apply something. See "The handler
;;; contract" in doc/kernel-tutorial.md.

(defstruct (descriptor (:constructor make-descriptor (name &key call-handler))
                       (:print-object print-descriptor))
  (name nil :type symbol)
  (call-handler nil))

(defun print-descriptor (d stream)
  (print-unreadable-object (d stream :type t)
    (princ (descriptor-name d) stream)))

(macrolet ((define-descriptors (&rest names)
             `(progn
                ,@(loop for n in names
                        collect `(defparameter ,(intern (format nil "*~A-DESCRIPTOR*" n))
                                   (make-descriptor ',(intern (string n) :keyword)))))))
  (define-descriptors fixnum float character symbol string nothing true
                      cons vector binding frame thread code fn primitive
                      descriptor unknown))

(defun descriptor-of (value)
  "Return VALUE's descriptor. The machine never interprets the result."
  (typecase value
    (null *nothing-descriptor*)
    ((eql t) *true-descriptor*)
    (fixnum *fixnum-descriptor*)
    (float *float-descriptor*)
    (character *character-descriptor*)
    (symbol *symbol-descriptor*)
    (string *string-descriptor*)
    (cons *cons-descriptor*)
    (simple-vector *vector-descriptor*)
    (binding *binding-descriptor*)
    (frame *frame-descriptor*)
    (thread *thread-descriptor*)
    (code *code-descriptor*)
    (fn *fn-descriptor*)
    (primitive *primitive-descriptor*)
    (descriptor *descriptor-descriptor*)
    (t *unknown-descriptor*)))

;;; ---------------------------------------------------------------------
;;; code
;;; ---------------------------------------------------------------------
;;; Instructions are fixed-width: a flat fixnum array with a stride of
;;; three -- opcode, arg1, arg2. Because an instruction word is a fixnum,
;;; operands that denote values are indices into CONSTANTS.
;;;
;;; FRAME-SIZE is N-LOCALS plus the maximum operand depth, so allocating
;;; a frame is adding a known constant. A compiler computes it; until
;;; there is one the assembler takes it as a parameter.

(defstruct (code (:constructor %make-code)
                 (:print-object print-code))
  (name nil)
  (instructions (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (constants #() :type simple-vector)
  (arity 0 :type fixnum)
  (n-locals 0 :type fixnum)
  (frame-size 0 :type fixnum))

(defun print-code (c stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~A arity ~D" (or (code-name c) "anonymous") (code-arity c))))

(defun code-length (code)
  "The number of instructions in CODE."
  (truncate (length (code-instructions code)) 3))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------
;;; A closure is code plus the frame it was created in. Because
;;; environments and frames are the same representation, the lexical
;;; chain is CAPTURED-FRAME links and needs nothing of its own.

(defstruct (fn (:constructor make-fn (code &optional captured-frame))
               (:print-object print-fn))
  code
  captured-frame)

(defun print-fn (f stream)
  (print-unreadable-object (f stream :type t)
    (princ (or (code-name (fn-code f)) "anonymous") stream)))

;;; ---------------------------------------------------------------------
;;; frames
;;; ---------------------------------------------------------------------
;;; The frame is the computation. PARENT is the continuation; SLOTS
;;; holds locals in the low positions and the operand stack above them,
;;; with SP marking the top of the operand area.

(defstruct (frame (:constructor %make-frame)
                  (:print-object print-frame))
  (parent nil)
  (fn nil)
  (pc 0 :type fixnum)
  (slots (make-array 0) :type simple-vector)
  (sp 0 :type fixnum))

(defun print-frame (f stream)
  (print-unreadable-object (f stream :type t)
    (format stream "~A pc ~D sp ~D"
            (let ((fn (frame-fn f)))
              (if fn (or (code-name (fn-code fn)) "anonymous") "-"))
            (frame-pc f)
            (frame-sp f))))

(defun make-frame (fn &key parent)
  "Allocate a frame for FN, sized from its code, linked to PARENT."
  (let ((code (fn-code fn)))
    (%make-frame :parent parent
                 :fn fn
                 :pc 0
                 :slots (make-array (code-frame-size code) :initial-element *nothing*)
                 :sp (code-n-locals code))))

(declaim (inline frame-push frame-pop frame-top))

(defun frame-push (frame value)
  (setf (svref (frame-slots frame) (frame-sp frame)) value)
  (incf (frame-sp frame))
  value)

(defun frame-pop (frame)
  (decf (frame-sp frame))
  (svref (frame-slots frame) (frame-sp frame)))

(defun frame-top (frame)
  (svref (frame-slots frame) (1- (frame-sp frame))))

(defun frame-operands (frame)
  "The operand area of FRAME as a list, bottom first. For inspection."
  (let ((code (fn-code (frame-fn frame))))
    (coerce (subseq (frame-slots frame) (code-n-locals code) (frame-sp frame))
            'list)))

;;; ---------------------------------------------------------------------
;;; bindings
;;; ---------------------------------------------------------------------
;;; A global name resolves to a binding, never directly to a value. That
;;; indirection is why a redefinition takes effect in code compiled
;;; before it. The unbound state is distinct from a NOTHING value: it is
;;; what the error hook detects.

(defstruct (binding (:constructor make-binding (name))
                    (:print-object print-binding))
  (name nil)
  (value *nothing*)
  (bound? nil :type boolean)
  (dynamic? nil :type boolean))

(defun print-binding (b stream)
  (print-unreadable-object (b stream :type t)
    (format stream "~A~:[ unbound~;~]" (binding-name b) (binding-bound? b))))

(defvar *globals* (make-hash-table :test #'equal)
  "Maps a name to its binding. Keyed by name string rather than by
symbol identity, for the same reason as *OPCODE-NUMBERS*: a program
should read the same whichever package it was typed in. Bard will have
its own symbols eventually.")

(defun global-binding (name)
  "The binding for NAME, created unbound if it does not yet exist."
  (let ((key (string name)))
    (or (gethash key *globals*)
        (setf (gethash key *globals*) (make-binding key)))))

;;; ---------------------------------------------------------------------
;;; primitives
;;; ---------------------------------------------------------------------
;;; Primitives are monomorphic, fixed-arity, and named with a leading
;;; underscore so they stay out of the language's namespace. All
;;; polymorphism lives above the machine.

(defstruct (primitive (:constructor make-primitive (name arity function))
                      (:print-object print-primitive))
  (name nil)
  (arity 0 :type fixnum)
  (function nil))

(defun print-primitive (p stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A/~D" (primitive-name p) (primitive-arity p))))

;;; ---------------------------------------------------------------------
;;; threads
;;; ---------------------------------------------------------------------
;;; A thread is a frame and a status -- barely a new thing. DYNENV holds
;;; this thread's dynamic rebindings and is almost always empty.

(defstruct (thread (:constructor make-thread (frame &key (status :ready)))
                   (:print-object print-thread))
  (frame nil)
  (status :ready)
  (result '())
  (dynenv '()))

(defun print-thread (th stream)
  (print-unreadable-object (th stream :type t)
    (format stream "~A" (thread-status th))))
