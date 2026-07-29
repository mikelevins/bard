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
  '((const       :const)
    (local       :num :num)
    (global      :binding)
    (set-global  :binding)
    (set-local   :num :num)
    (drop)
    (goto        :label)
    (branch-false :label)
    (close       :code)
    (call        :count)
    (tailcall    :count)
    (return      :count)
    (recv        :count)
    (recv-all)
    (yield))
    "Each entry is (NAME . OPERAND-KINDS), in opcode order."))

(macrolet ((define-opcodes ()
             `(progn
                ,@(loop for spec in (symbol-value '*opcode-specs*)
                        for i from 0
                        collect `(defconstant ,(intern (format nil "+OP-~A+" (first spec)))
                                   ,i)))))
  (define-opcodes))

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
  "The human-readable name of opcode number OP."
  (if (and (integerp op) (< -1 op (length *opcode-names*)))
      (svref *opcode-names* op)
      (format nil "?~A" op)))

(defun opcode-number (name)
  (or (gethash (string name) *opcode-numbers*)
      (error "No such instruction: ~S" name)))

;;; ---------------------------------------------------------------------
;;; the assembler
;;; ---------------------------------------------------------------------

;;; ASSEMBLE takes a list in which a bare symbol is a label and a list
;;; is an instruction, and produces a code object. Operands are written
;;; as what they mean -- a literal value, a global's name, a label --
;;; and the assembler interns them into the constants vector.
;;;
;;;   (assemble '((const 42) (return 1)))
;;;
;;;   (assemble '((global n) (const 3) (global <) (call 2) (recv 1)
;;;               (branch-false else)
;;;               (const "small") (goto done)
;;;               else (const "big")
;;;               done (return 1)))

(defun assemble (forms &key name (arity 0) (n-locals 0) frame-size)
  "Assemble FORMS into a code object."
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
                  :n-locals n-locals
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

#+repl (disassemble (assemble '((const 42) (return 1)) :name "answer"))
