;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; stacks
;;; ---------------------------------------------------------------------

(define (make-vm-stack #!optional (size 512))
  (make-stretchy-vector size))

(define (stack-push! s v)
  (vector-push-extend! s v))

(define (stack-pop! s)
  (vector-pop! s))

;;; ---------------------------------------------------------------------
;;; opcodes
;;; ---------------------------------------------------------------------

(define HALT 0)
(define LVAR 1)
(define LSET 2)
(define MVAR 3)
(define MSET 4)
(define SLOT 5)
(define SSET 6)
(define JUMP 7)
(define TJUMP 8)
(define FJUMP 9)
(define ARGS 10)
(define METHOD 11)
(define DISP 12)
(define SAVE 13)
(define APPLY 14)
(define PRIM 15)
(define RESTORE 16)
(define CONST 17)
(define NOTHING 18)
(define TRUE 19)
(define FALSE 20)
(define ONE 21)
(define TWO 22)
(define NEG1 23)
(define NOP 24)

;;; ---------------------------------------------------------------------
;;; vm
;;; ---------------------------------------------------------------------

(define (image-toplevel image)
  (vector-ref image 1))

(define (image-environment image)
  (vector-ref image 2))

(define (image-globals image)
  (vector-ref image 3))

(define (print-stack stack)
  (let loop ((i (- (stretchy-vector-fill-pointer stack) 1)))
    (if (>= i 0)
        (begin
          (display "   ")(display (stretchy-vector-ref stack i))
          (loop (- i 1))))))

(define $instruction-names
  (list->vector '(HALT LVAR LSET MVAR MSET SLOT SSET
                       JUMP TJUMP FJUMP ARGS METHOD DISP
                       SAVE APP PRIM RESTORE CONST NOTHING
                       TRUE FALSE ONE TWO NEG1)))

(define $instruction-argcounts
  (list->vector '(0 LVAR LSET MVAR MSET SLOT SSET
                       JUMP TJUMP FJUMP ARGS METHOD DISP
                       SAVE APP PRIM RESTORE CONST NOTHING
                       TRUE FALSE ONE TWO NEG1)))

(define (print-instruction code index)
  (let* ((iname (vector-ref $instruction-names (vector-ref code index)))
         (nargs (vector-ref $instruction-argcounts index)))
    (display iname)
    (let loop ((i 0))
      (if (< i nargs)
          (begin
            (display " ")
            (display (vector-ref code (+ index 1 i))))))))

(define (print-code code)
  (let ((len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (let ((nargs (vector-ref $instruction-argcounts (vector-ref code i))))
            (display "   ")(print-instruction code i)
            (loop (+ i 1 nargs)))))))

(define (vm image #!optional (stack-size 512))
  (call/cc (lambda (return)
             (let ((code (image-toplevel image))
                   (pc 0)
                   (env (image-environment image))
                   (globals (image-globals image))
                   (stack (make-vm-stack)))
               (letrec ((printvm (lambda ()
                                   (newline)
                                   (display "   pc: ")(display pc)(newline)
                                   (display "stack:")(newline)(print-stack stack)
                                   (display " code:")(newline)(print-code code)
                                   (newline)(newline)))
                        (halt (lambda () (return (printvm))))
                        (lvar (lambda () #f))
                        (lset (lambda () #f))
                        (mvar (lambda () #f))
                        (mset (lambda () #f))
                        (slot (lambda () #f))
                        (sset (lambda () #f))
                        (jump (lambda () #f))
                        (tjump (lambda () #f))
                        (fjump (lambda () #f))
                        (args (lambda () #f))
                        (method (lambda () #f))
                        (disp (lambda () #f))
                        (save (lambda () #f))
                        (app (lambda () #f))
                        (prim (lambda () #f))
                        (restore (lambda () #f))
                        (const (lambda () #f))
                        (nothing (lambda () #f))
                        (true (lambda () #f))
                        (false (lambda () #f))
                        (one (lambda () #f))
                        (two (lambda () #f))
                        (neg1 (lambda () #f)))
                 (let* ((jumpbuf (vector halt lvar lset mvar mset slot sset
                                         jump tjump fjump args method disp
                                         save app prim restore const nothing
                                         true false one two neg1))
                        (fetch (lambda (opc)(vector-ref jumpbuf opc)))
                        (exec (lambda (op)(op))))
                   (let loop ()
                     (exec (fetch (vector-ref code pc))))))))))

(define (test-image code)
  (vector #f code (vector)(vector)))

;;; (vm (test-image (vector HALT)))

