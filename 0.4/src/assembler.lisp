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

(defun assemble (instructions)
  (multiple-value-bind (length labels)
      (asm-first-pass instructions)
    (asm-second-pass instructions length labels)))

(defun asm-first-pass (code)
  (let ((length 0)
        (labels nil))
    (dolist (instr code)
      (if (label? instr)
          (push (cons instr length) labels)
          (incf length)))
    (values length labels)))

(defun asm-second-pass (code length labels)
  (let ((addr 0)
        (code-vector (make-array length)))
    (dolist (instr code)
      (unless (label? instr)
        (if (is instr '(GO TGO FGO SAVE))
            (setf (cdr instr)
                  (list (cdr (assoc (arg1 instr) labels)))))
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
