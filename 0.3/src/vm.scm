;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard VM
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define (opcode instr)(car instr))

(define (false? x)(eqv? x #f))
(define (unbound? x)(eqv? x #!unbound))

(define (logically-false? x)
  (or (false? x)
      (null? x)
      (unbound? x)))

(define (apply-primitive prim args) #f)


(define machine (toplevel)
  (let* ((fn toplevel)
         (code (function-code toplevel))
         (pc 0)
         (env '())
         (globals (make-table test: eq?))
         (stack '())
         (nargs 0)
         (instr '()))
    ;; machine operations
    (letrec ((code-ref (lambda (n)(list-ref code n)))
             (incpc! (lambda ()(set! pc (+ pc 1))))
             (push! (lambda (x)(set! stack (cons x stack))))
             (pop! (lambda ()(let ((v (car stack)))(set! stack (cdr stack)) v)))
             (popn! (lambda (n)(let loop ((i (- n 1))
                                          (result '()))
                                 (if (< i 0)
                                     (reverse result)
                                     (loop (- i 1)(cons (pop!) result))))))
             (top (lambda ()(car stack)))
             (second (lambda ()(cadr stack)))
             (env-ref (lambda (i j)(list-ref (list-ref env i) j)))
             (env-set! (lambda (i j v))(list-set! (list-ref env i) j v))
             (pushenv! (lambda (x)(set! env (cons x env))))
             (popenv! (lambda ()(let ((e (car env)))(set! env (cdr env)) e)))
             (setpc! (lambda (x)(set! pc x)))
             (gref (lambda (g)(table-ref globals g #!unbound)))
             (gset! (lambda (g v)(table-set! globals g v)))
             (arg1 (lambda ()(list-ref instr 1)))
             (arg2 (lambda ()(list-ref instr 2)))
             (arg3 (lambda ()(list-ref instr 3))))
      (call/cc
       (lambda (return)
         (let loop ()
           (set! instr (code-ref pc))
           (incpc!)
           (case (opcode instr)
             
             ;; Variable/stack manipulation instructions:
             ((LREF)   (push! (env-ref (arg1)(arg2))))
             ((LSET)   (push! (env-set! (arg1)(arg2)(arg3))))
             ((GREF)   (push! (gref (arg1))))
             ((GSET)   (push! (gset! (arg1)(arg2))))
             ((POP)    (pop!))
             ((CONST)  (push! (arg1)))
             
             ;; Branching instructions:
             ((JUMP)   (setpc! (arg1)))
             ((FJUMP)  (if (logically-false? (pop!))(setpc! (arg1))))
             ((TJUMP)  (if (logically-true? (pop!))(setpc! (arg1))))

             ;; Function call/return instructions:
             ((SAVE)   (push! (make-return-record pc: (arg1) fn: fn :env env)))
             ((RETURN) ;; return value is top of stack; ret-addr is second
              (set! fn (return-record-fn (second)))
              (set! code (function-code fn))
              (set! env (return-record-env (second)))
              (set! pc (return-record-pc (second)))
              ;; Get rid of the return address, but keep the value
              (set! stack (cons (top)(cddr stack))))

             ((CALLJ)  (popenv!)
              (set! fn  (pop!))
              (set! code (function-code f))
              (set! env (function-env f))
              (set! pc 0)
              (set! nargs (arg1)))

             ((ARGS)   (assert (= nargs (arg1))
                               (str "Wrong number of arguments:"
                                    (arg1) " expected, "
                                    nargs " supplied"))
              ;; collect required args into a list and push it onto the env
              (let loop ((i nargs)
                         (result '()))
                (if (<= i 0)
                    (pushenv! (reverse result))
                    (let ((v (pop!)))
                      (loop (- i 1)(cons v result))))))

             ((ARGS.)  (assert (= nargs (arg1))
                               (str "Wrong number of arguments:"
                                    (arg1) " or more expected, "
                                    nargs " supplied"))
              ;; (arg1) is the number of required args
              ;; nargs is the actual number of args passed
              ;; collect optional args into a restarg, collect the
              ;; required args, put the restarg at the end, push the
              ;; result onto the env
              (let ((required-count (arg1)))
                (let loop ((i nargs)
                           (restarg '()))
                  (if (<= i required-count)
                      (let ((restarg (reverse restarg)))
                        (let loop2 ((i i)
                                    (args '()))
                          (if (<= i 0)
                              (pushenv! (reverse (cons restarg args)))
                              (loop 2 (- i 1)(cons (pop!) args)))))
                      (loop (- i 1)(cons (pop!) restarg))))))
             
             ((FN)     (push! (make-function code: (function-code (arg1)) env: env)))

             ((PRIM)   (push! (apply-primitive (arg1)(popn! nargs))))
             
             ;; Continuation instructions:
             ((SET-CC) (set! stack (top)))

             ((CC)     (push! (make-function
                               env: (list (list stack))
                               code: '((ARGS 1) (LREF 1 0 ";" stack) (SET-CC)
                                       (LREF 0 0) (RETURN)))))
             
             ;; Other:
             ((HALT) (return (top)))

             ;; Oops!
             (else (error (str "Unknown opcode: " instr))))))))))

