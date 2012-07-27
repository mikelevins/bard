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

(define (opcode->opname opc)
  (vector-ref $opnames-table opc))

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

(define-macro (%makeop args . body)
  `(lambda (,(car args))
     (lambda ,(cdr args)
       ,@body)))

;;; constants

(defop   1 CONST (%makeop (vm k)(vm-push-val! vm k)))
(defop   2 NOTHING (%makeop (vm)(vm-push-val! vm $nothing)))
(defop   3 MINUSONE (%makeop (vm)(vm-push-val! vm -1)))
(defop   4 ZERO (%makeop (vm)(vm-push-val! vm 0)))
(defop   5 ONE (%makeop (vm)(vm-push-val! vm 1)))
(defop   6 TWO (%makeop (vm)(vm-push-val! vm 2)))

;;; variables

(defop   7 LVAR (%makeop (vm i j)(vm-push-val! vm (lref (vm-env vm) i j))))
(defop   8 LSET (%makeop (vm i j v)(lset! (vm-env vm) i j v)))
(defop   9 MODULE (%makeop (vm nm)(vm-push-val! vm (vm-get-module-name vm nm))))
(defop  10 ADDMODULE (%makeop (vm nm)(vm-add-module vm nm)))
(defop  11 MVAR (%makeop (vm s)(vm-push-val! vm (mref (vm-modules vm)(vm-pop-val! vm) s))))
(defop  12 MSET (%makeop (vm s)(vm-push-val! vm (mset! (vm-modules vm) (vm-pop-val! vm) s (vm-pop-val! vm)))))
(defop  13 POP (%makeop (vm)(vm-pop-val! vm)))





