;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          system.scm
;;;; Project:       Bard
;;;; Purpose:       supporting system and diagnostic tools for the vm
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; 
;;; ----------------------------------------------------------------------

(define (asm . instructions)
  (make-codevector
   (list->vector
    (map (lambda (i)
           (let* ((opnm (car i))
                  (opcode (opcode opnm))
                  (opargs (cdr i)))
             (cons opcode opargs)))
         instructions))))

(define (link . cvec)
  (vector-map (lambda (i)
                (let ((opc (car i))
                      (op (vmoperator opc))
                      (opargs (cdr i)))
                  (cons op opargs)))
              (codevector-code cvec))) 

(define (display-instruction ins)
  (let* ((opc (instruction-opcode ins))
         (nm (opname opc)))
    (display nm)
    (for-each (lambda (arg)
                (display " ")
                (display arg))
              (instruction-args ins))))

(define (display-function fn)
  (display fn))

(define (showvm state)
  (newline)
  (display "Bard VM 0.40")(newline)
  (display "          pc: ")(display (vmstate-pc state))(newline)
  (display " instruction: ")(if (< (vmstate-pc state)
                                   (codevector-length (fn-body (vmstate-function state))))
                                (display-instruction (codevector-ref (fn-body (vmstate-function state)) (vmstate-pc state))))
  (newline)
  (display "    function: ")(display-function (vmstate-function state))(newline)
  (display "       nvals: ")(display (vmstate-nvals state))(newline)
  (display "       stack: ")
  (for-each (lambda (item)
              (newline)
              (display "          ")
              (display item))
            (vmstate-stack state))
  (newline)
  (display " environment: ")
  (for-each (lambda (binding)
              (newline)
              (display "          ")
              (display (car binding))
              (display ": ")
              (display (cdr binding)))
            (vmstate-env state))
  (newline)
  (display "     globals: ")
  (table-for-each (lambda (key val)
                    (newline)
                    (display "          ")
                    (display key)
                    (display ": ")
                    (display val))
                  (vmstate-globals state))
  (newline)
  (display "     program: ")
  (vector-for-each (lambda (instr)
                     (newline)
                     (display "          ")
                     (display-instruction instr))
                   (codevector-code (fn-body (vmstate-function state)))) 
  (newline))