;;;; ***********************************************************************
;;;;
;;;; Name:          opcodes.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the instruction set, the assembler, the disassembler
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; Fifteen instructions. See doc/kernel.md §3.
;;;
;;; Instructions are fixed-width with a stride of three fixnums --
;;; opcode, arg1, arg2 -- so no instruction has more than two operands
;;; and decoding never branches.
;;;
;;; Nothing here should ever require looking up what a number means:
;;; every instruction has a name, every operand has a kind, and
;;; DISASSEMBLE renders both.

;;; ---------------------------------------------------------------------
;;; the instruction set
;;; ---------------------------------------------------------------------

;;; Operand kinds, used by the assembler to encode and by the
;;; disassembler to render:
;;;
;;;   :const    an index into the code object's constants
;;;   :binding  an index into constants, holding a binding
;;;   :code     an index into constants, holding a code object
;;;   :label    an instruction index
;;;   :count    a plain number of values
;;;   :num      a plain number

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *opcode-specs*
  '((op_CONST        :const)
    (op_LOCAL         :num :num)
    (op_GLOBAL        :binding)
    (op_SET-GLOBAL    :binding)
    (op_SET-LOCAL     :num :num)
    (op_DROP)
    (op_GOTO          :label)
    (op_BRANCH-FALSE  :label)
    (op_CLOSE         :code)
    (op_CALL          :count)
    (op_TAILCALL      :count)
    (op_RETURN        :count)
    (op_RECV          :count)
    (op_RECV-ALL)
    (op_YIELD))
    "Each entry is (NAME . OPERAND-KINDS), in opcode order."))

;;; Instruction names carry an op_ sentinel so that none can collide with
;;; a Common Lisp symbol. See doc/decisions.md §10.1. Enforced here rather than remembered:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (spec *opcode-specs*)
    (let ((name (string (first spec))))
      (unless (eql 0 (search "OP_" name))
        (error "Instruction ~A lacks the op_ sentinel." name))
      (when (nth-value 1 (find-symbol name :common-lisp))
        (error "Instruction ~A collides with a Common Lisp symbol." name)))))

(defparameter *opcode-names*
  (coerce (mapcar #'first *opcode-specs*) 'simple-vector)
  "Opcode number to name. The reason you never have to remember a number.")

(defparameter *opcode-arg-kinds*
  (coerce (mapcar #'rest *opcode-specs*) 'simple-vector))

(defparameter *opcode-numbers*
  (let ((table (make-hash-table :test #'equal)))
    (loop for spec in *opcode-specs*
          for i from 0
          do (setf (gethash (string (first spec)) table) i))
    table)
  "Instruction name to opcode number. Keyed by name rather than by
symbol identity, so that instructions written in any package read the
same. Bard will have its own symbols eventually; until then this keeps
the assembler from caring which package a program was typed in.")

(defun opcode-name (op)
  "The name of opcode number OP, written as it appears in source: a
lowercase op_ sentinel and an uppercase instruction name. The reader
upcases the whole symbol, so the sentinel has to be restored here for
disassembly to read back the way it was written."
  (if (and (integerp op) (< -1 op (length *opcode-names*)))
      (let ((name (symbol-name (svref *opcode-names* op))))
        (concatenate 'string "op_" (subseq name 3)))
      (format nil "?~A" op)))

(defun opcode-number (name)
  (or (gethash (string name) *opcode-numbers*)
      (error "No such instruction: ~S" name)))

;;; ---------------------------------------------------------------------
;;; dispatching
;;; ---------------------------------------------------------------------
;;; Dispatch wants a jump table, which wants literal integers as keys.
;;; This resolves instruction names to those integers at macroexpansion
;;; time, by string, so the package a clause was written in does not
;;; matter and a misspelling is an error rather than a clause that never
;;; fires. See doc/decisions.md §10.2.

(defmacro dispatch-on-opcode (op &body clauses)
  "Dispatch on OP by instruction name. Each clause is (NAME . BODY), or
(T . BODY) for the fallback. Expands to CASE with numeric keys."
  `(case ,op
     ,@(loop for (key . body) in clauses
             collect (if (eq key t)
                         `(t ,@body)
                         `(,(opcode-number key) ,@body)))))

;;; ---------------------------------------------------------------------
;;; the assembler
;;; ---------------------------------------------------------------------

;;; ASSEMBLE takes a list in which a bare symbol is a label and a list
;;; is an instruction, and produces a code object. Operands are written
;;; as what they mean -- a literal value, a global's name, a label --
;;; and the assembler interns them into the constants vector.
;;;
;;;   (assemble '((op_CONST 42) (op_RETURN 1)))
;;;
;;;   (assemble '((op_GLOBAL n) (op_CONST 3) (op_GLOBAL <) (op_CALL 2) (op_RECV 1)
;;;               (op_BRANCH-FALSE else)
;;;               (op_CONST "small") (op_GOTO done)
;;;               else (op_CONST "big")
;;;               done (op_RETURN 1)))

(defun assemble (forms &key name (arity 0) rest? (n-locals nil) frame-size)
  "Assemble FORMS into a code object. N-LOCALS defaults to ARITY, since
op_CALL places the arguments in the low slots and a function with no
further locals needs exactly that many."
  (setf n-locals (or n-locals (+ arity (if rest? 1 0))))
  (when (< n-locals (+ arity (if rest? 1 0)))
    (error "~A declares ~D local~:P but takes ~D argument~:P."
           (or name "anonymous") n-locals arity))
  (let ((labels (make-hash-table :test #'eq))
        (constants (make-array 0 :adjustable t :fill-pointer t))
        (instructions '())
        (index 0))
    ;; pass one: label positions
    (dolist (form forms)
      (if (symbolp form)
          (setf (gethash form labels) index)
          (incf index)))
    ;; pass two: emit
    (flet ((intern-constant (value)
             (or (position value constants :test #'eql)
                 (vector-push-extend value constants)))
           (label-index (l)
             (if (integerp l)
                 l
                 (or (gethash l labels)
                     (error "No such label: ~S" l)))))
      (dolist (form forms)
        (unless (symbolp form)
          (destructuring-bind (op . args) form
            (let ((kinds (svref *opcode-arg-kinds* (opcode-number op)))
                  (encoded '()))
              (unless (= (length args) (length kinds))
                (error "~S takes ~D operand~:P, given ~D." op (length kinds) (length args)))
              (loop for arg in args
                    for kind in kinds
                    do (push (ecase kind
                               (:const (intern-constant arg))
                               (:binding (intern-constant (global-binding arg)))
                               (:code (intern-constant arg))
                               (:label (label-index arg))
                               ((:count :num) arg))
                             encoded))
              (setf encoded (nreverse encoded))
              (push (list (opcode-number op)
                          (or (first encoded) 0)
                          (or (second encoded) 0))
                    instructions))))))
    (let* ((words (loop for ins in (nreverse instructions) append ins))
           (array (make-array (length words) :element-type 'fixnum
                                             :initial-contents words)))
      (%make-code :name name
                  :instructions array
                  :constants (coerce constants 'simple-vector)
                  :arity arity
                  :rest? rest?
                  :n-locals n-locals
                  ;; A compiler computes FRAME-SIZE as N-LOCALS plus the
                  ;; maximum operand depth. There is no compiler yet, so
                  ;; this is a generous stand-in -- a placeholder, not a
                  ;; design decision. Nothing checks for overflow: a
                  ;; program deep enough to exceed it fails as a host
                  ;; array error rather than a machine one.
                  :frame-size (or frame-size (+ n-locals 32))))))

;;; ---------------------------------------------------------------------
;;; the disassembler
;;; ---------------------------------------------------------------------

(defun operand-string (kind value code)
  (ecase kind
    (:const (format nil "~S" (svref (code-constants code) value)))
    (:binding (format nil "~A" (binding-name (svref (code-constants code) value))))
    (:code (let ((c (svref (code-constants code) value)))
             (format nil "~A" (or (code-name c) "anonymous"))))
    (:label (format nil "~D" value))
    ((:count :num) (format nil "~D" value))))

(defun instruction-string (code index)
  "A human-readable rendering of instruction INDEX of CODE."
  (let* ((ins (code-instructions code))
         (base (* 3 index))
         (op (aref ins base))
         (kinds (svref *opcode-arg-kinds* op))
         (args (list (aref ins (+ base 1)) (aref ins (+ base 2)))))
    (format nil "~A~{ ~A~}"
            (opcode-name op)
            (loop for kind in kinds
                  for arg in args
                  collect (operand-string kind arg code)))))

(defun disassemble (code &key (stream *standard-output*))
  "Print CODE as instructions with names and meaningful operands."
  (format stream "~&~A  arity ~D  locals ~D  frame ~D~%"
          (or (code-name code) "anonymous")
          (code-arity code) (code-n-locals code) (code-frame-size code))
  (dotimes (i (code-length code))
    (format stream "~4D: ~A~%" i (instruction-string code i)))
  (values))

#+repl (disassemble (assemble '((op_CONST 42) (op_RETURN 1)) :name "answer"))
       ; prints: answer  arity 0  locals 0  frame 32
       ;            0: op_CONST 42
       ;            1: op_RETURN 1
