
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; code generation
;;; ---------------------------------------------------------------------

(define-macro (%gen opname . args)
  `(list (list ',opname ,@args)))

(define (%seq . code)
  (apply append code))

(define (%assemble-instruction instr)
  (cons (opname->opcode (car instr))
        (cdr instr)))

(define (%assemble code)
  (apply vector
         (map %assemble-instruction code)))

(define (%disassemble-instruction instr)
  (let ((op (car instr)))
    (cons (if (number? op)
              (opcode->opname op)
              (op->opname op))
          (cdr instr))))

(define (%disassemble code)
  (let ((len (vector-length code)))
    (let loop ((i 0)
               (result '()))
      (if (< i len)
          (loop (+ 1 i)
                (cons (%disassemble-instruction (vector-ref code i)) result))
          (reverse result)))))

;;; ---------------------------------------------------------------------
;;; instruction utilities
;;; ---------------------------------------------------------------------

(define (%op instr)(car instr))
(define (%args instr)(cdr instr))

(define (%link-instruction! instr)
  (set-car! instr (opcode->op (%op instr)))
  instr)

(define (%link! code)
  (let ((len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (%link-instruction! (vector-ref code i))
            (loop (+ 1 i)))
          code))))

;;; ---------------------------------------------------------------------
;;; the compiler
;;; ---------------------------------------------------------------------

(define (%compile-constant expr)
  (cond
   ((%nothing? expr)(%gen NOTHING))
   ((zero? expr)(%gen ZERO))
   ((eqv? 1 expr)(%gen ONE))
   ((eqv? 2 expr)(%gen TWO))
   ((eqv? -1 expr)(%gen MINUSONE))
   (else (%gen CONST expr))))

(define (%compile-var-ref var env)
  (let ((ref (in-env? var env)))
    (if ref
        (%gen LVAR (car ref)(cdr ref))
        (receive (varname mname)(parse-symbol-name var)
                 (%seq (%gen MODULE mname)
                       (%gen MVAR var))))))

(define (%compile-apply expr env)
  (%gen APPLY))

(define (%compile-function expr env)
  (let ((code (%link! (%compile expr env))))
    (make-function code)))

(define (%compile-application expr env)
  (cond
   ((%special-form? (car expr))(%compile-special-form expr env))
   ((%macro? (car expr))(%compile (%macroexpand expr) env))
   (else (%compile-apply expr env))))

(define (%compile expr env #!key (assemble #t))
  (let ((code (cond
               ((%nothing? expr)(%compile-constant expr))
               ((%symbol? expr)(%compile-var-ref expr env))
               ((%list? expr)(%compile-application expr env))
               (else (%compile-constant expr)))))
    (if assemble
        (%assemble code)
        code)))


;;; constants
;;; (%disassemble (%compile (%read-from-string "nothing") '()))
;;; (%disassemble (%compile (%read-from-string "-1") '()))
;;; (%disassemble (%compile (%read-from-string "0") '()))
;;; (%disassemble (%compile (%read-from-string "1") '()))
;;; (%disassemble (%compile (%read-from-string "2") '()))
;;; (%disassemble (%compile (%read-from-string "12") '()))

;;; variables
;;; (%disassemble (%compile (%read-from-string "y") '(((x . 0)))))
;;; (%disassemble (%compile (%read-from-string "y") '(((x . 0)(y . 1)))))


