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

(define-type vm 
  function
  code
  pc
  env
  globals
  stack
  nargs
  instr
  halted)

(define code-ref (lambda (vm n)(list-ref (vm-code vm) n)))
(define incpc! (lambda (vm)(vm-pc-set! vm (+ (vm-pc vm) 1))))
(define push! (lambda (vm x)(vm-stack-set! vm (cons x (vm-stack vm)))))
(define pop! (lambda (vm) (let ((v (car (vm-stack vm))))(vm-stack-set! (cdr (vm-stack vm))) v)))
(define popn! (lambda (vm n)
                (let loop ((i (- n 1))
                           (result '()))
                  (if (< i 0)
                      (reverse result)
                      (loop (- i 1)(cons (pop! vm) result))))))
(define top (lambda (vm)(car (vm-stack vm))))
(define second (lambda (vm)(cadr (vm-stack vm))))
(define env-ref (lambda (vm i j)(vector-ref (list-ref (vm-env vm) i) j)))
(define env-set! (lambda (vm i j v)(vector-set! (list-ref (vm-env vm) i) j v)))
(define pushenv! (lambda (vm x)(vm-env-set! vm (cons x (vm-env vm)))))
(define popenv! (lambda (vm)(let ((e (car (vm-env vm))))(vm-env-set! vm (cdr (vm-env vm))) e)))
(define setpc! (lambda (vm x)(vm-pc-set! vm x)))
(define gref (lambda (vm g)(table-ref (vm-globals vm) g #!unbound)))
(define gset! (lambda (vm g v)(table-set! (vm-globals vm) g v)))
(define arg1 (lambda (vm)(list-ref (vm-instr vm) 1)))
(define arg2 (lambda (vm)(list-ref (vm-instr vm) 2)))
(define arg3 (lambda (vm)(list-ref (vm-instr vm) 3)))

(define $opcode->opfn-table (make-table test: eq?))
(define $opfn->opcode-table (make-table test: eq?))

(define (defop opcode opfn)
  (table-set! $opcode->opfn-table opcode opfn)
  (table-set! $opfn->opcode-table opfn opcode)
  opcode)

;;; (LREF i j)
(defop 'LREF (lambda (vm)(push! vm (env-ref vm (arg1 vm)(arg2 vm)))))

;;; (LSETR i j)
(defop 'LSETR (lambda (vm)(push! vm (env-setter vm (arg1 vm)(arg2 vm)))))

;;; (GREF gv)
(defop 'GREF (lambda (vm)(push! vm (gref vm (arg1 vm)))))

;;; (GSETR gv)
(defop 'GSETR (lambda (vm)(push! vm (gsetter vm (arg1 vm)))))

;;; (POP)
(defop 'POP (lambda (vm)(pop! vm)))

;;; (CONST c)
(defop 'CONST (lambda (vm)(push! vm (arg1 vm))))

;;; (JUMP dst)
(defop 'JUMP (lambda (vm)(setpc! vm (arg1 vm))))

;;; (FJUMP dst)
(defop 'FJUMP (lambda (vm)(if (logically-false? (pop! vm))(setpc! vm (arg1 vm)))))

;;; (TJUMP dst)
(defop 'TJUMP (lambda (vm)(if (logically-true? (pop! vm))(setpc! vm (arg1 vm)))))

;;; (SAVE continue)
(defop 'SAVE (lambda (vm)(push! vm (make-return-record pc: (arg1 vm) fn: (vm-function vm) :env (vm-env vm)))))

;;; (RETURN) 
(defop 'RETURN ; return value is top of stack; ret-addr is second
  (lambda (vm)
    (vm-function-set! vm (return-record-fn (second vm)))
    (vm-code-set! vm (function-code (vm-function vm)))
    (vm-env-set! vm (return-record-env (second vm)))
    (vm-pc-set! vm (return-record-pc (second vm)))
    ;; Get rid of the return address, but keep the value
    (vm-stack-set! vm (cons (top vm)(cddr (vm-stack vm))))))

;;; (CALLJ argcount)
(defop 'CALLJ (lambda (vm)
               (popenv! vm)
               (vm-function-set! vm (pop! vm))
               (vm-code-set! vm (function-code (vm-function vm)))
               (vm -env-set! vm (function-env (vm-function vm)))
               (vm-pc-set! vm 0)
               (vm-nargs-set! vm (arg1 vm))))

;;; (ARGS argcount)
(defop 'ARGS (lambda (vm)
              (assert (= (vm-nargs vm) (arg1 vm))
                      (str "Wrong number of arguments:"
                           (arg1 vm) " expected, "
                           (vm-nargs vm) " supplied"))
              ;; collect required args into a list and push it onto the env
              (let loop ((i (vm-nargs vm))
                         (result '()))
                (if (<= i 0)
                    (pushenv! vm (list->vector (reverse result)))
                    (let ((v (pop! vm)))
                      (loop (- i 1)(cons v result)))))))

;;; (ARGS. argcount)
(defop 'ARGS. (lambda (vm)
               (assert (= (vm-nargs vm) (arg1 vm))
                       (str "Wrong number of arguments:"
                            (arg1 vm) " or more expected, "
                            (vm-nargs vm) " supplied"))
               ;; (arg1) is the number of required args
               ;; nargs is the actual number of args passed
               ;; collect optional args into a restarg, collect the
               ;; required args, put the restarg at the end, push the
               ;; result onto the env
               (let ((required-count (arg1 vm)))
                 (let loop ((i (vm-nargs vm))
                            (restarg '()))
                   (if (<= i required-count)
                       (let ((result (list->vector (reverse restarg))))
                         (let loop2 ((i i)
                                     (args '()))
                           (if (<= i 0)
                               (pushenv! vm (reverse (cons result args)))
                               (loop 2 (- i 1)(cons (pop! vm) args)))))
                       (loop (- i 1)(cons (pop! vm) restarg)))))))

;;; (FN fn)
(defop 'FN (lambda (vm)(push! vm (make-function code: (function-code (arg1 vm)) env: (vm-env vm)))))

;;; (PRIM p)
(defop 'PRIM (lambda (vm)(push! vm (apply-primitive (arg1 vm)(popn! vm (vm-nargs vm))))))

;;; (SET-CC)
(defop 'SET-CC (lambda (vm)(vm-stack-set! vm (top vm))))

;;; (CC)
(defop 'CC (lambda (vm)
            (push! vm
                   (make-function
                    env: (list (vector (vm-stack vm)))
                    code: '((ARGS 1) (LREF 1 0 ";" stack) (SET-CC)
                            (LREF 0 0) (RETURN))))))

;;; (HALT)
(defop 'HALT (lambda (vm)(vm-halted-set! vm #t)))
