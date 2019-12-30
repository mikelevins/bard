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

       ;; branches
       ((= opc JUMP) (begin (set-pc! vm (arg1 vm))
                            (loop)))

       ((= opc FJUMP) (begin
                        (when (false? (stack-pop! vm))
                          (set-pc! vm (arg1 vm)))
                        (loop)))
       
       ((= opc TJUMP) (begin
                        (when (true? (stack-pop! vm))
                          (set-pc! vm (arg1 vm)))
                        (loop)))

       ;; function calling
       ((= opc SAVE) (begin (stack-push! vm (make-return (arg1 vm) (vm-fn vm) (vm-env vm)))
                            (loop)))

       ;; top of stack is the return value
       ;; second on stack is the return record
       ((= opc RETURN) (let* ((val #f)
                              (return #f))
                         (set! val (stack-pop! vm))
                         (set! return (stack-pop! vm))
                         (vm-push! vm val)
                         (vm-fn-set! vm (return-fn return))
                         (vm-code-set! vm (fn-code (return-fn return)))
                         (vm-env-set! vm (return-env return))
                         (vm-pc-set! vm (return-pc return))
                         (loop)))

       ((= opc CALLJ) (begin (env-pop! (vm-env vm))
                             (let ((f (stack-pop! vm)))
                               (vm-code-set! vm (fn-code f))
                               (vm-env-set! vm (fn-env f))
                               (vm-pc-set! vm 0)
                               (vm-nargs-set! vm (arg1 vm)))
                             (loop)))

       ((= opc ARGS) (let ((found-argcount (vm-nargs vm))
                           (expected-argcount (arg1 vm)))
                       (ensure-argcount found-argcount expected-argcount)
                       (let ((frame (make-env-frame (stack-npop! vm expected-argcount))))
                         (env-push! vm frame))
                       (loop)))

       ((= opc ARGS&REST) (let ((found-argcount (vm-nargs vm))
                                (expected-argcount (arg1 vm)))
                            (ensure-argcount>= found-argcount expected-argcount)
                            (let* ((required-args (stack-npop! vm expected-argcount))
                                   (rest-args (stack-npop! vm (- found-argcount expected-argcount)))
                                   (frame (make-env-frame (add-last required-args rest-args))))
                              (env-push! vm frame))
                            (loop)))

       ;; *** here's where rewriting the instruction execution last stopped
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
       ((= opc ADD) (begin (loop)))
       ((= opc SUB) (begin (loop)))
       ((= opc MUL) (begin (loop)))
       ((= opc DIV) (begin (loop)))
       ((= opc LTE) (begin (loop)))
       ((= opc GT) (begin (loop)))
       ((= opc LT) (begin (loop)))
       ((= opc GTE) (begin (loop)))
       ((= opc EQ) (begin (loop)))
       ((= opc EQL) (begin (loop)))
       ((= opc EQUAL) (begin (loop)))
       ((= opc CONS) (begin (loop)))
       ((= opc LIST2) (begin (loop)))
       ((= opc NAME!) (begin (loop)))
       
       ;; ternary builtins
       ((= opc LIST3) (begin (loop)))

       ;; constants
       ((= opc TRUE) (begin (loop)))
       ((= opc FALSE) (begin (loop)))
       ((= opc NIL) (begin (loop)))
       ((= opc MINUS-ONE) (begin (loop)))
       ((= opc ZERO) (begin (loop)))
       ((= opc ONE) (begin (loop)))
       ((= opc TWO) (begin (loop)))

       ;; machine control
       ((= opc HALT) (vm:top stack))
       (else (error (string-append "Unknown opcode: "
                                   (object->string instr))))))))
