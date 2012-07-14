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
;;; instruction utils
;;; ---------------------------------------------------------------------

(define (arg1 instr)(list-ref instr 1))
(define (arg2 instr)(list-ref instr 2))

;;; ---------------------------------------------------------------------
;;; ops
;;; ---------------------------------------------------------------------

(define (%NIL Stack Env Code Dump)
  (-> ((cons nil Stack) Env (cdr Code) Dump)))

(define (%LDC Stack Env Code Dump)
  (let* ((instr (car Code))
         (c (arg1 instr)))
    (-> ((cons c Stack) Env (cdr Code) Dump))))

(define (%LD Stack Env Code Dump)
  (let* ((instr (car Code))
         (i (arg1 instr))
         (j (arg2 instr))
         (v (list-ref (list-ref Env i) j)))
    (-> ((cons v Stack) Env (cdr Code) Dump))))

(define (%SEL Stack Env Code Dump)
  (let* ((instr (car Code))
         (v (car Stack))
         (ct (arg1 instr))
         (cf (argr instr)))
    (if (true? v)
        (-> (Stack Env ct (cons (cdr Code) Dump)))
        (-> (Stack Env cf (cons (cdr Code) Dump))))))

(define (%JOIN Stack Env Code Dump)
  (-> (Stack Env (car Dump) (cdr Dump))))
