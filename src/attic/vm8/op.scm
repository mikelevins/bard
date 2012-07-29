;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          op.scm
;;;; Project:       Bard VM
;;;; Purpose:       vm operations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $max-opcode 25)

(define $optable (make-vector (+ $max-opcode 1) #f))
(define $opnames-table (make-vector (+ $max-opcode 1) #f))

(define-macro (defop opcode opname opfn)
  `(begin
     (define ,opname ,opcode)
     (vector-set! $optable ,opcode ,opfn)
     (vector-set! $opnames-table ,opcode ',opname)))

(define (opcode->op opc)(vector-ref $optable opc))

(define (op->opcode op)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? op (vector-ref $optable i))
            i
            (loop (+ 1 i)))
        #f)))

(define (op->opname op)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? op (vector-ref $optable i))
            (vector-ref $opnames-table i)
            (loop (+ 1 i)))
        #f)))

(define (opname->opcode opnm)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? opnm (vector-ref $opnames-table i))
            i
            (loop (+ 1 i)))
        #f)))

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

;;; halting the vm

(defop 0 HALT (lambda (vm) #f)) ; dummy op; gets replaced at vm init time

;;; variables and values

(defop  1 LVAR (lambda (vm i j) (push-val! vm (lvar-ref vm i j))))
(defop  2 LSET (lambda (vm i j) (lvar-set! vm i j (pop-val! vm))))
(defop  3 MVAR (lambda (vm mname varname) (push-val! vm (mvar-ref mname varname))))
(defop  4 MSET (lambda (vm mname varname) (mvar-set! vm mname varname (pop-val! vm))))

(defop  5 SLOT (lambda (vm) 
                 (let ((sname (pop-val! vm))
                       (obj (pop-val! vm)))
                   (push-val! vm (slot-ref obj sname)))))

(defop  6 SSET (lambda (vm) 
                 (let ((val (pop-val! vm))
                       (sname (pop-val! vm))
                       (obj (pop-val! vm)))
                   (slot-set! obj sname val)
                   (push-val! vm val))))

(defop  7 POP  (lambda (vm) (pop-val! vm)))

;;; constants

(defop  8 CONST   (lambda (vm k) (push-val! vm k)))
(defop  9 NOTHING (lambda (vm) (push-val! vm $nothing)))
(defop 10 TRUE    (lambda (vm) (push-val! vm $true)))
(defop 11 FALSE   (lambda (vm) (push-val! vm $false)))
(defop 12 ZERO    (lambda (vm) (push-val! vm 0)))
(defop 13 ONE     (lambda (vm) (push-val! vm 1)))
(defop 14 TWO     (lambda (vm) (push-val! vm 2)))
(defop 15 NEG1    (lambda (vm) (push-val! vm -1)))

;;; functions

(defop 16 METH (lambda (vm)
                 (let ((mbody (pop-val! vm))
                       (arglist (pop-val! vm)))
                   (push-val! vm (make-method arglist mbody (vm-env vm))))))

(defop 17 FN (lambda (vm) (push-val! vm (makefn))))

(defop 18 DEFM (lambda (vm)
                 (let ((body (pop-val! vm))
                       (sig (pop-val! vm)))
                   (push-val! (make-method sig body (vm-env vm))))))

(defop 19 DISP (lambda (vm)
                 (let ((args (pop-val! vm))
                       (f (pop-val! vm)))
                   (push-val! vm args)
                   (push-val! (best-applicable-method f args)))))

;;; function-calling and control

(defop 20 IF (lambda (vm thend elsed) 
               (if (pop-val! vm)
                   (vm-pc-set! vm thend)
                   (vm-pc-set! vm elsed))))

(defop 21 ARGS (lambda (vm) 
                 (let* ((method (top-val vm))
                        (params (msig method))
                        (vals (take-vals vm (length params))))
                   (extend-env! vm params vals))))

(defop 22 APPLY (lambda (vm) 
                  (vm-code-set! vm (method-code (pop-val! vm)))
                  (vm-pc-set! vm 0)))

(defop 23 SAVE (lambda (vm) (push-state! vm)))

(defop 24 RESTORE (lambda (vm) (pop-state! vm)))

(defop 25 PRIM (lambda (vm pcode) 
                 (let* ((p (pcode->prim pcode))
                        (argcount (vmprim-nargs p))
                        (args (vm-vals vm)))
                   (vm-require-argcount vm argcount)
                   (clear-vals! vm)
                   (push-val! vm (apply-prim vm p args)))))
