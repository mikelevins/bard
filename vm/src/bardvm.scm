;;;; ***********************************************************************
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard VM main program
;;;; Author:        mikel evins
;;;; Copyright:     2019 by mikel evins
;;;;
;;;; ***********************************************************************

(define (fn-code fn) 'not-yet-implemented)
(define (fn-name fn) 'not-yet-implemented)
(define (set-fn-name! fn nm) 'not-yet-implemented)
(define (top stack) 'not-yet-implemented)
(define (first stack) 'not-yet-implemented)
(define (second stack) 'not-yet-implemented)
(define (third stack) 'not-yet-implemented)
(define (drop n stack) 'not-yet-implemented)
(define (opcode instr) 'not-yet-implemented)
(define (arg1 instr) 'not-yet-implemented)
(define (arg2 instr) 'not-yet-implemented)
(define (env-ref env frame-index var-index) 'not-yet-implemented)
(define (env-set! env frame-index var-index val) 'not-yet-implemented)
(define (global-ref varname) 'not-yet-implemented)
(define (global-set! varname val) 'not-yet-implemented)

(define (machine fn)
  (let* ((code (fn-code fn))
         (pc 0)
         (env '())
         (stack '())
         (n-args 0)
         (instr '()))
    (let loop ()
      (set! instr (list-ref code pc))
      (set! pc (+ 1 pc))

      (let ((opc (opcode instr)))
        (cond 
         ;; variables and the stack
         ((= opc LVAR) (let* ((frame-index (arg1 instr))
                              (var-index (arg2 instr))
                              (val (env-ref env frame-index var-index)))
                         (set! stack (cons val stack))
                         (loop)))
         ((= opc LSET) (let* ((frame-index (arg1 instr))
                              (var-index (arg2 instr))
                              (val (top stack)))
                         (env-set! env frame-index var-index val)
                         (loop)))
         ((= opc GVAR) (let* ((varname (arg1 instr))
                              (val (global-ref varname)))
                         (set! stack (cons val stack))
                         (loop)))
         ((= opc GSET) (let* ((varname (arg1 instr))
                              (val (top stack)))
                         (global-set! varname val)
                         (loop)))
         ((= opc POP) (begin (if (null? stack)
                                 (error "Stack underflow")
                                 (set! stack (cdr stack)))
                             (loop)))
         ((= opc CONST) (begin (set! stack
                                     (cons (arg1 instr) stack))
                               (loop)))

         ;; branches
         ((= opc JUMP) (begin (set! pc (arg1 instr))(loop)))
         ((= opc FJUMP) (let ((testval #f))
                          (if (null? stack)
                              (error "Stack underflow")
                              (begin
                                (set! testval (top stack))
                                (set! stack (cdr stack))
                                (when (not testval)
                                  (set! pc (arg1 instr)))))
                          (loop)))
         ((= opc TJUMP) (let ((testval #f))
                          (if (null? stack)
                              (error "Stack underflow")
                              (begin
                                (set! testval (top stack))
                                (set! stack (cdr stack))
                                (when testval
                                  (set! pc (arg1 instr)))))
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
                                   (cons (+ (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc SUB) (begin (set! stack
                                   (cons (- (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc MUL) (begin (set! stack
                                   (cons (* (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc DIV) (begin (set! stack
                                   (cons (/ (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc LTE) (begin (set! stack
                                   (cons (<= (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc GT) (begin (set! stack
                                  (cons (> (second stack)(first stack))
                                        (drop 2 stack)))
                            (loop)))
         ((= opc LT) (begin (set! stack
                                  (cons (< (second stack)(first stack))
                                        (drop 2 stack)))
                            (loop)))
         ((= opc GTE) (begin (set! stack
                                   (cons (>= (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc EQ) (begin (set! stack
                                  (cons (eq? (second stack)(first stack))
                                        (drop 2 stack)))
                            (loop)))
         ((= opc EQL) (begin (set! stack
                                   (cons (eqv? (second stack)(first stack))
                                         (drop 2 stack)))
                             (loop)))
         ((= opc EQUAL) (begin (set! stack
                                     (cons (equal? (second stack)(first stack))
                                           (drop 2 stack)))
                               (loop)))
         ((= opc CONS) (begin (set! stack
                                    (cons (cons (second stack)(first stack))
                                          (drop 2 stack)))
                              (loop)))
         ((= opc LIST2) (let ((val (list (first stack)
                                         (second stack))))
                          (set! stack (cons val (drop 2 stack)))
                          (loop)))
         ((= opc NAME!) (begin (set-fn-name! (second stack)(first stack))
                               (set! stack (cons (second stack)(drop 2 stack)))
                               (loop)))
         
         ;; ternary builtins
         ((= opc LIST3) (let ((val (list (first stack)
                                         (second stack)
                                         (third stack))))
                          (set! stack(cons val (drop 3 stack)))
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
         ((= opc HALT) (top stack))
         (else (error (string-append "Unknown opcode: "
                                     (object->string instr)))))))))
