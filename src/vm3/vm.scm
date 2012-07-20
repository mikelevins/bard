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
;;; register utils
;;; ---------------------------------------------------------------------

(define (drop n ls)(list-tail ls n))

;;; ---------------------------------------------------------------------
;;; instruction utils
;;; ---------------------------------------------------------------------

(define (arg1 instr)(list-ref instr 1))
(define (arg2 instr)(list-ref instr 2))

;;; ---------------------------------------------------------------------
;;; ops
;;; ---------------------------------------------------------------------

(define (%NOP Stack Env Code Dump)
  (-> (Stack Env Code Dump)))

(define (%NIL Stack Env Code Dump)
  (-> ((cons nil Stack) Env (drop 1 Code) Dump)))

(define (%LDC Stack Env Code Dump)
  (let* ((c (arg1 Code)))
    (-> ((cons c Stack) Env (drop 2 Code) Dump))))

(define (%LD Stack Env Code Dump)
  (let* ((ref (arg1 Code))
         (i (car ref))
         (j (cdr ref))
         (v (list-ref (list-ref Env i) j)))
    (-> ((cons v Stack) Env (drop 2 Code) Dump))))

(define (%SEL Stack Env Code Dump)
  (let* ((v (car Stack))
         (ct (arg1 Code))
         (cf (arg2 Code)))
    (if (true? v)
        (-> ((drop 1 Stack) Env ct (cons (drop 3 Code) Dump)))
        (-> ((drop 1 Stack) Env cf (cons (drop 3 Code) Dump))))))

(define (%JOIN Stack Env Code Dump)
  (-> (Stack Env (car Dump) (drop 1 Dump))))

(define (%LDF Stack Env Code Dump)
  (let* ((f (arg1 Code))
         (closure (cons f Env)))
    (-> ((cons closure Stack) Env (drop 2 Code) Dump))))

(define (%AP Stack Env Code Dump)
  (let* ((closure (car Stack))
         (f (car closure))
         (e+ (cdr closure))
         (vals (cadr Stack))
         (s (drop 2 Stack))
         (e Env)
         (c (drop 1 Code))
         (d Dump))
    (-> (nil (cons vals e+) f (cons s (cons e (cons c d)))))))

(define (%RTN Stack Env Code Dump)
  (let* ((s (car Dump))
         (e (cadr Dump))
         (c (caddr Dump))
         (d (cdddr Dump))
         (val (car Stack)))
    (-> ((cons val s) e c d))))

(define (%CONS Stack Env Code Dump)
  (let* ((hd (car Stack))
         (tl (cadr Stack))
         (pair (cons hd tl)))
    (-> ((cons pair (drop 2 Stack)) Env (drop 1 Code) Dump))))


(define $vmops (make-vector 16 0))

(define NOP 0)
(define NIL 1)
(define LDC 2)
(define LD 3)
(define SEL 4)
(define JOIN 5)
(define LDF 6)
(define AP 7)
(define RTN 8)
(define CONS 9)

(define (defop op fn)
  (vector-set! $vmops op fn))

(defop NOP %NOP)
(defop NIL %NIL)
(defop LDC %LDC)
(defop LD %LD)
(defop SEL %SEL)
(defop JOIN %JOIN)
(defop LDF %LDF)
(defop AP %AP)
(defop RTN %RTN)
(defop CONS %CONS)

(define (%fetch ins)(vector-ref $vmops ins))

(define (%exec s e c d)
  ((%fetch (car c)) s e c d))

;;; (%exec '() '() `(,NOP) '())
;;; (%exec '() '() `(,NIL) '())
;;; (%exec '() '() `(,LDC 2) '())
;;; (%exec '() '((3)) `(,LD (0 . 0)) '())
;;; if #f 1 else 2 nop
;;; (%exec '(#f) '() `(,SEL (,LDC 1 ,JOIN)(,LDC 2 ,JOIN) ,NOP) '())
;;; (%exec '() '() `(,LDC 2 ,JOIN) '((0)))
;;; (%exec '(2) '() `(,JOIN) '((0)))
;;; (%exec '(2) '() `(,NOP) '())
;;;
;;; ((lambda (x y)(cons x y)) 2 3)
;;; NIL LDC 3 CONS LDC 2 CONS LDF (LD (0 . 1) LD (0 . 0) CONS RTN) AP
;;; (%exec '() '() `(,NIL ,LDC 3 ,CONS ,LDC 2 ,CONS ,LDF (,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) ,AP) '())
;;; (%exec '(3 ()) '() `(,CONS ,LDC 2 ,CONS ,LDF (,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) ,AP) '())
;;; (%exec '((3)) '() `(,LDC 2 ,CONS ,LDF (,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) ,AP) '())
;;; (%exec '(2 (3)) '() `(,CONS ,LDF (,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) ,AP) '())
;;; (%exec '((2 3)) '() `(,LDF (,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) ,AP) '())
;;; (%exec `(((,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN)) (2 3)) '() `(,AP) '())
;;; (%exec `() '((2 3)) `(,LD (0 . 1) ,LD (0 . 0) ,CONS ,RTN) '(()()()))
;;; (%exec `(3) '((2 3)) `(,LD (0 . 0) ,CONS ,RTN) '(()()()))
;;; (%exec `(2 3) '((2 3)) `(,CONS ,RTN) '(()()()))
;;; (%exec `((2 . 3)) '((2 3)) `(,RTN) '(()()()))
;;;




