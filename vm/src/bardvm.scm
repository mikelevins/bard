;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM main program
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(define-structure vm fn code pc env stack n-args instr)

(define (fetch-next-instr! vm) )
(define (inc-pc! vm) )
(define (current-opcode vm) )
(define (stack-push! vm val) )
(define (stack-pop! vm) )
(define (stack-top vm) )
(define (arg1 vm) )
(define (arg2 vm) )
(define (arg3 vm) )
(define (global-ref vm var) )
(define (global-set! vm var val) )

(define (runvm vm)
  (let loop ()
    (fetch-next-instr! vm)
    (inc-pc! vm)

    (let ((opc (current-opcode vm)))
      (cond 
       ;; variables and the stack
       ((= opc LVAR) (let ((var (arg1 vm)))
                       (push! vm (env-ref (vm-env vm) var))
                       (loop)))

       ((= opc LSET) (let ((var (arg1 vm)))
                       (env-set! (vm-env vm) var (stack-top vm))
                       (loop)))
       
       ((= opc GVAR) (let ((var (arg1 vm)))
                       (stack-push! vm (global-ref vm var))
                       (loop)))
       
       ((= opc GSET) (let* ((var (arg1 vm))
                            (val (stack-top vm)))
                       (global-set! vm var val)
                       (loop)))
       
       ((= opc POP) (begin (stack-pop! vm)
                           (loop)))
       
       ((= opc CONST) (begin (stack-push! vm (arg1 vm))
                             (loop)))

       ;; *** here's where rewriting the instruction execution last stopped
       ;; branches
       ((= opc JUMP) (begin (set! pc (instr-arg1 instr))(loop)))
       ((= opc FJUMP) (let ((testval #f))
                        (if (null? stack)
                            (error "Stack underflow")
                            (begin
                              (set! testval (vm:top stack))
                              (set! stack (cdr stack))
                              (when (not testval)
                                (set! pc (instr-arg1 instr)))))
                        (loop)))
       ((= opc TJUMP) (let ((testval #f))
                        (if (null? stack)
                            (error "Stack underflow")
                            (begin
                              (set! testval (vm:top stack))
                              (set! stack (cdr stack))
                              (when testval
                                (set! pc (instr-arg1 instr)))))
                        (loop)))

       ;; function calling
       ((= opc SAVE) (begin (loop)))
       ((= opc RETURN) (begin (loop)))
       ((= opc CALLJ) (begin (loop)))
       ((= opc ARGS) (begin (loop)))
       ((= opc ARGS&REST) (begin (loop)))
       ((= opc FN) (begin (loop)))
       ((= opc PRIM) (begin (loop)))

       ;; continuations
       ((= opc SETCC) (begin (loop)))
       ((= opc CC) (begin (loop)))

       ;; nullary builtins
       ((= opc BARD-READ) (begin (loop)))
       ((= opc NEWLINE) (begin (loop)))

       ;; unary builtins
       ((= opc CAR) (begin (loop)))
       ((= opc CDR) (begin (loop)))
       ((= opc CADR) (begin (loop)))
       ((= opc NOT) (begin (loop)))
       ((= opc LIST1) (begin (loop)))
       ((= opc COMPILER) (begin (loop)))
       ((= opc DISPLAY) (begin (loop)))
       ((= opc WRITE) (begin (loop)))
       ((= opc RANDOM) (begin (loop)))

       ;; binary builtins
       ((= opc ADD) (begin (set! stack
                                 (cons (+ (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc SUB) (begin (set! stack
                                 (cons (- (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc MUL) (begin (set! stack
                                 (cons (* (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc DIV) (begin (set! stack
                                 (cons (/ (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc LTE) (begin (set! stack
                                 (cons (<= (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc GT) (begin (set! stack
                                (cons (> (vm:second stack)(vm:first stack))
                                      (drop stack 2)))
                          (loop)))
       ((= opc LT) (begin (set! stack
                                (cons (< (vm:second stack)(vm:first stack))
                                      (drop stack 2)))
                          (loop)))
       ((= opc GTE) (begin (set! stack
                                 (cons (>= (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc EQ) (begin (set! stack
                                (cons (eq? (vm:second stack)(vm:first stack))
                                      (drop stack 2)))
                          (loop)))
       ((= opc EQL) (begin (set! stack
                                 (cons (eqv? (vm:second stack)(vm:first stack))
                                       (drop stack 2)))
                           (loop)))
       ((= opc EQUAL) (begin (set! stack
                                   (cons (equal? (vm:second stack)(vm:first stack))
                                         (drop stack 2)))
                             (loop)))
       ((= opc CONS) (begin (set! stack
                                  (cons (cons (vm:second stack)(vm:first stack))
                                        (drop stack 2)))
                            (loop)))
       ((= opc LIST2) (let ((val (list (vm:first stack)
                                       (vm:second stack))))
                        (set! stack (cons val (drop stack 2)))
                        (loop)))
       ((= opc NAME!) (begin (fn-name-set! (vm:second stack)(vm:first stack))
                             (set! stack (cons (vm:second stack)(drop stack 2)))
                             (loop)))
       
       ;; ternary builtins
       ((= opc LIST3) (let ((val (list (vm:first stack)
                                       (vm:second stack)
                                       (vm:third stack))))
                        (set! stack(cons val (drop stack 3)))
                        (loop)))

       ;; constants
       ((= opc TRUE) (begin (set! stack (cons #t stack))(loop)))
       ((= opc FALSE) (begin (set! stack (cons #f stack))(loop)))
       ((= opc NIL) (begin (set! stack (cons '() stack))(loop)))
       ((= opc MINUS-ONE) (begin (set! stack (cons -1 stack))(loop)))
       ((= opc ZERO) (begin (set! stack (cons 0 stack))(loop)))
       ((= opc ONE) (begin (set! stack (cons 1 stack))(loop)))
       ((= opc TWO) (begin (set! stack (cons 2 stack))(loop)))

       ;; machine control
       ((= opc HALT) (vm:top stack))
       (else (error (string-append "Unknown opcode: "
                                   (object->string instr))))))))
