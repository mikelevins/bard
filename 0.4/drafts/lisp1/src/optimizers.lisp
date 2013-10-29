;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          optimizers.lisp
;;;; Project:       Bard
;;;; Purpose:       code-optimizers for the compiler
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; a peephole optimizer for assembly code
;;; ---------------------------------------------------------------------

(defun optimize (code)
  (let ((any-change nil))
    ;; Optimize each tail  
    (loop for code-tail on code do
         (setf any-change (or (optimize-1 code-tail code)
                              any-change)))
    ;; If any changes were made, call optimize again
    (if any-change
        (optimize code)
        code)))

(defun optimize-1 (code all-code)
  ;; Data-driven by the opcode of the first instruction
  (let* ((instr (first code))
         (optimizer (get-optimizer (opcode instr))))
    (when optimizer
      (funcall optimizer instr code all-code))))

;;; ---------------------------------------------------------------------
;;; defining optimizers
;;; ---------------------------------------------------------------------

(let ((optimizers (make-hash-table :test #'eql)))

  (defun get-optimizer (opcode)
    (gethash opcode optimizers))

  (defun put-optimizer (opcode fn)
    (setf (gethash opcode optimizers) fn)))

(defun gen1 (&rest args) "Generate a single instruction" args)
(defun target (instr code) (second (member (arg1 instr) code)))
(defun next-instr (code) (find-if (complement #'label-p) code))

(defmacro def-optimizer (opcodes args &body body)
  "Define assembly language optimizers for these opcodes."
  (assert (and (listp opcodes) (listp args) (= (length args) 3)))
  `(dolist (op ',opcodes)
     (put-optimizer op #'(lambda ,args .,body))))

;;; ---------------------------------------------------------------------
;;; defined optimizers
;;; ---------------------------------------------------------------------

(def-optimizer (:LABEL) (instr code all-code)
  ;; ... L ... => ... ... ;if no reference to L
  (when (not (find instr all-code :key #'arg1))
    (setf (first code) (second code)
          (rest code) (rest2 code))
    t))

(def-optimizer (JUMP CALL CALLJ RETURN) (instr code all-code)
  (declare (ignore all-code))
  ;; (JUMP L1) ...dead code... L2 ==> (JUMP L1) L2
  (setf (rest code) (member-if #'label-p (rest code)))
  ;; (JUMP L1) ... L1 (JUMP L2) ==> (JUMP L2)  ... L1 (JUMP L2)
  (when (and (is instr 'JUMP)
             (is (target instr code) '(JUMP RETURN))
    (setf (first code) (copy-list (target instr code)))
    t)))

(def-optimizer (TGO FGO) (instr code all-code)
  (declare (ignore all-code))
  ;; (FGO L1) ... L1 (GO L2) ==> (FGO L2) ... L1 (GO L2)
  (when (is (target instr code) 'GO)
    (setf (second instr) (arg1 (target instr code)))
    t))



