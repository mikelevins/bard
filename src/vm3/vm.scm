;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm -- experimental variation 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; further experiments in a high-level VM
;;; this time combining my dataflow function-based machine
;;; with landin's SECD architecture
;;;
;;; registers are Stack, Environment, Code, and (value) Dump
;;; all are lists

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------

(define true #t)
(define false #f)
(define nil '())

(define (false? x)(not x))
(define (true? x)(not (false? x)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define-macro (-> args . funs)
  (let ((gen-vars (lambda (n)
                    (let loop ((i 0)
                               (result '()))
                      (if (>= i n)
                          result
                          (loop (+ i 1)(cons (gensym) result)))))))
    (if (null? funs)
        `(values ,@args)
        (let ((f (car funs))
              (more-funs (cdr funs))
              (vars (gen-vars (length args))))
          (if (null? more-funs)
              `(receive ,vars (,f ,@args)(values ,@vars))
              `(receive ,vars (,f ,@args)
                        (-> ,vars ,@more-funs)))))))

(define-macro (%asm1 form)
  `(vector ,(car form) ,@(cdr form)))

(define-macro (%asm forms)
  (cons 'vector
        (map (lambda (f)`(%asm1 ,f))
             forms)))

;;; ---------------------------------------------------------------------
;;; multiple return values
;;; ---------------------------------------------------------------------

(define val0 (lambda (v . rest) v))
(define val1 (lambda (v w . rest) w))
(define val2 (lambda (v w x . rest) x))
(define val3 (lambda (v w x y . rest) y))
(define valn (lambda (n . vals) (list-ref vals n)))
(define vals (lambda vals vals))

;;; ---------------------------------------------------------------------
;;; ops
;;; ---------------------------------------------------------------------

(define (opHALT Stack Env Code Dump)
  (-> (Stack Env nil nil)))

(define (opNIL Stack Env Code Dump)
  (-> ((cons nil Stack) Env Code Dump)))

(define (opCONST Stack Env Code Dump)
  (let ((x (instruction:arg 1 instr)))
    (-> ((cons x Stack) Env Code Dump))))

(define (opTRUE Stack Env Code Dump)
  (-> ((cons true Stack) Env Code Dump)))

(define (opFALSE Stack Env Code Dump)
  (-> ((cons false Stack) Env Code Dump)))

(define (opSEL Stack Env Code Dump)
  (if (true? (car Stack))
      (-> ((cdr Stack) Env (instruction:arg1 (car Code)) (cons (cdr Code) Dump)))
      (-> ((cdr Stack) Env (instruction:arg2 (car Code)) (cons (cdr Code) Dump)))))

(define (opJOIN Stack Env Code Dump)
  (-> (Stack Env (car Dump) (cdr Dump))))

(define (opFN Stack Env Code Dump)
  (let ((fn (instruction:arg1 (car Code))))
    (-> ((cons (cons fn Env) Stack) Env Code Dump))))




