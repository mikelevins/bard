;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (gambit-scheme)
         (standard-bindings)
         (extended-bindings)
         (block))

;;; macros

(define-macro (prog1 expr . body)
  (let ((var (gensym)))
    `(let ((,var expr))
       ,@body
       ,var)))

;;; opcodes
;;; bytecodes that represent VM instructions. at link time the
;;; bytecode is replaced by the opfn that implements it.

(define HALT    0)
(define CONST   1)
(define LREF    2)
(define LSET    3)
(define GREF    4)
(define GSET    5)
(define POPV    6)
(define POPC    7)
(define PRIM    8)
(define JUMP    9)
(define FJUMP  10)
(define TJUMP  11)
(define SAVE   12)
(define CALL   13)
(define RETURN 14)
(define CC     15)
(define SETCC  16)

;;; vm helpers

(define (standard-bard-environment) #f) ; make a standard starting env for bard (most likely the null env)
(define (standard-bard-globals) #f) ; initialize the VM's globals
(define (env-ref env i j) #f) ; fetch a lexical variable's value
(define (env-set! env i j val) #f) ; set a lexical variable's value
(define (global-ref env g) #f) ; fetch a global variable's value
(define (global-set! globals g val) #f) ; set a global variable's value
(define (false? x) #f) ; test for #f or other logically false values (e.g. undefined, nothing)
(define (true? x) #f) ; test for logically true values
(define (link-function-code f instructions) #f) ; replace instruction bytecodes with opfns
(define (prim-argcount-check p vals) #f) ; check whether the supplies values satisfy the prim's argcount requirements
(define (prim-argcount-err p nvals) #f) ; construct an error message about failure to satisfy the prim's argcount requirements
(define (prim-function p) #f) ; fetch the function that imp,ements the primitive
(define (assert test msg) #f) ; signal an error if the test returns #f
(define (prim-apply p vals) ; apply a primitive and return any values produced
  (call-with-values (lambda ()(apply prim-function vals))
    (lambda outvals outvals)))

;;; run the vm

(define (vmrun function)
  (call/cc
   (lambda (exit)
     (let ((fn #f)
           (instr #f)
           (code #f)
           (pc 0)
           (nvals 0)
           (env (standard-bard-environment))
           (globals (standard-bard-globals))
           (vals '())
           (stack '()))
       (letrec (;; vm instructions
                (%halt   exit)
                (%pushv  (lambda (k)(set! vals (cons k vals))(incnvals! 1)))
                (%popv   (lambda ()(prog1 (car vals)(%decnvals! 1)(set! vals (cdr vals)))))
                (%lref   (lambda (i j)(%pushv (env-ref env i j))))
                (%lset   (lambda (i j)(env-set! env i j (%popv))))
                (%gref   (lambda (g)(%pushv! (global-ref env g))))
                (%gset   (lambda (g)(global-set! globals g (%popv))))
                (%popc   (lambda ()(prog1 (car stack)(set! stack (cdr stack)))))
                (%prim   (lambda (p)(_check-prim-argc p)(_applyprim p)))
                (%jump   (lambda (d)(set! pc d)))
                (%fjump  (lambda (d)(if (false? (car vals))(set! pc d))))
                (%tjump  (lambda (d)(if (true? (car vals))(set! pc d))))
                (%cc     (lambda ()(set! vals (cons (_makecc) vals))))
                (%setcc  (lambda ()(_setcc! (%popv))))
                ;; instruction table
                (instructions (vector %halt %pushv %popv %lref %lset %gref %gset %popc %prim %jump %fjump %tjump %cc %setcc))
                ;; continuations
                (_makecc (lambda ()       #f))
                (_setcc! (lambda (cc)     #f))
                (_ccref (lambda (cc k)    #f))
                (_vm-set-cc! (lambda (cc) #f))
                ;; modifying vm state
                (_initvm! (lambda ()(set! fn (link-function-code f instructions))(set! code (function-code fn))))
                (_incnvals! (lambda (n)(set! nvals (+ nvals n))))
                (_decnvals! (lambda (n)(set! nvals (- nvals n))))
                (_pushnvals! (lambda (ks)(set! vals (append ks vals))(incnvals! (length ks))))
                (_takenvals! (lambda (n)(prog1 (take n vals)(%decnvals! n)(set! vals (drop n vals)))))
                (_pushc! (lambda (c)(set! stack (cons c stack))))
                (_check-prim-argc (lambda (p)(assert (prim-argcount-check p nvals)(prim-argcount-err p nvals))))
                (_applyprim (lambda (p)(_pushnvals! (prim-apply p (_takenvals! nvals)))))
                ;; decoding and executing instructions
                (_fetch! (lambda ()(set! instr (code-ref code pc))
                                 (set! pc (+ 1 pc))))
                (_exec! (lambda ()(apply (car instr)(cdr instr)))))
         (begin 
           (%initvm!)
           (let loop ()
             (%fetch!)
             (%exec!)
             (loop))))))))

;;; toplevel stuff

(define *bard-prompt* "bardvm> ")
