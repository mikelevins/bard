;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          assembler.lisp
;;;; Project:       Bard
;;;; Purpose:       the assembler for compiled bard code
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; assembler
;;; ---------------------------------------------------------------------

(defun assemble (fn)
  "Turn a list of instructions into a vector."
  (multiple-value-bind (length labels)
      (asm-first-pass (mfn-code fn))
    (setf (mfn-code fn)
          (asm-second-pass (mfn-code fn)
                           length labels))
    fn))

(defun asm-first-pass (code)
  "Return the labels and the total code length."
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label-p instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  "Put code into code-vector, adjusting for labels."
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label-p instr)
        (if (is instr '(JUMP TJUMP FJUMP SAVE))
            (set-arg1 instr (cdr (assoc (arg1 instr) labels))))
        (setf (aref code-vector addr) instr)
        (incf addr)))
    code-vector))

;;; ---------------------------------------------------------------------
;;; code-vector utils
;;; ---------------------------------------------------------------------

(defmethod show ((code vector) &optional (stream *standard-output*))
  (let ((len (length code)))
    (format stream "  code:~%")
    (dotimes (i len)
      (let ((instr (elt code i)))
        (format stream "    ~a~%" instr)))))

(defmethod show ((code cons) &optional (stream *standard-output*))
  (let ((len (length code)))
    (format stream "  code:~%")
    (dotimes (i len)
      (let ((instr (elt code i)))
        (format stream "    ~a~%" instr)))))
