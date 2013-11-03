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
(defun target (instr code) (second (member (first (args instr)) code)))
(defun next-instr (code) (find-if (complement #'label?) code))

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
  (when (not (find instr all-code :key #'(lambda (x)(first (args x)))))
    (setf (first code) (second code)
          (rest code) (drop 2 code))
    t))

(def-optimizer (GO CALL CALLJ RETURN) (instr code all-code)
  (declare (ignore all-code))
  ;; (GO L1) ...dead code... L2 ==> (GO L1) L2
  (setf (rest code) (member-if #'label? (rest code)))
  ;; (GO L1) ... L1 (JUMP L2) ==> (GO L2)  ... L1 (GO L2)
  (when (and (is instr 'JUMP)
             (is (target instr code) '(JUMP RETURN))
    (setf (first code) (copy-list (target instr code)))
    t)))

(def-optimizer (TGO FGO) (instr code all-code)
  (declare (ignore all-code))
  ;; (FGO L1) ... L1 (GO L2) ==> (FGO L2) ... L1 (GO L2)
  (when (is (target instr code) 'GO)
    (setf (second instr) (first (args (target instr code))))
    t))



