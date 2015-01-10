(define HALT 0) 
(define LREF 1)
(define LSET 2)
(define GREF 3)
(define GSET 4)
(define POP 5)
(define CONST 6)
(define GO 7)
(define TGO 8)
(define FGO 9)
(define SAVE 10)
(define RETURN 11)
(define CALL 12)
(define ARGS 13)
(define ARGS. 14)
(define FN 15)
(define PRIM 16)
(define SETCC 17)
(define CC 18)

(define (vmfetch! vm)
  (vm-instruction-set! vm
                       (code-ref (vm-code vm)
                                 (vm-pc vm))))

(define (incpc! vm)
  (vm-pc-set! vm (+ 1 (vm-pc vm))))


(define (pcset! vm dest)
  (vm-pc-set! vm dest))

(define (vm-cc-code)
  (vector `(,ARGS 1)
          `(,LREF 1 0)
          `(,SETCC)
          `(,LREF 0 0)
          `(,RETURN)))

(define (stackpush! vm val)
  (vm-stack-set! vm
                 (cons val
                       (vm-stack vm))))

(define (stackpop! vm)
  (let ((val (car (vm-stack vm))))
    (vm-stack-set! vm (cdr (vm-stack vm)))
    val))

(define (stacktop vm)
  (car (vm-stack vm)))

(define (stacktake! vm n)
  (let loop ((result '())
             (i n))
    (if (<= i 0)
        (reverse result)
        (loop (cons (stackpop! vm)
                    result)
              (- i 1)))))

(define (env-ref vm i j)
  (vector-ref (list-ref (vm-env vm) i) j))

(define (env-set! vm i j val)
  (vector-set! (list-ref (vm-env vm) i) j val))

(define (global-ref vm var)
  (let ((binding (assq var (vm-globals vm))))
    (if binding
        (cdr binding)
        (error "Unbound global variable" var))))

(define (global-set! vm var val)
  (let ((binding (assq var (vm-globals vm))))
    (if binding
        (set-cdr! binding val)
        (vm-globals-set! vm (cons (cons var val)
                                  (vm-globals vm))))))

(define (pcset! vm pc)
  (vm-pc-set! vm pc))

(define (true? x)
  (not (false? x)))

(define (false? x)
  (eqv? x #f))

(define (save-state vm pc)
  (make-saved-state pc
                    (vm-fn vm)
                    (vm-env vm)))

(define (restore-state! vm state)
  (vm-pc-set! vm (saved-state-pc state))
  (vm-fn-set! vm (saved-state-fn state))
  (vm-env-set! vm (saved-state-env state)))

(define (envpush! vm frame)
  (vm-env-set! vm
               (cons frame
                     (vm-env vm))))

(define (envpop! vm)
  (let ((frame (car (vm-env vm))))
    (vm-env-set! vm (cdr (vm-env vm)))
    frame))

;;; a primop is a function of the form
;;; (lambda (vm . args) ...)
;;; it may alter the state of the vm
(define (lookup-primop vm prim)
  )

(define (apply-prim vm prim args)
  (let ((primop (lookup-primop vm prim)))
    (primop vm args)))

(define (exec-halt! vm)
  (vm-halted?-set! vm #t))

(define (exec-lref! vm)
  (stackpush! vm (env-ref vm (arg 1 vm)(arg 2 vm))))

(define (exec-lset! vm)
  (env-set! vm (arg 1 vm)(arg 2 vm)(stackpop! vm)))

(define (exec-gref! vm)
  (stackpush! vm (global-ref vm (arg 1 vm)(arg 2 vm))))

(define (exec-gset! vm)
  (global-set! vm (arg 1 vm)(stackpop! vm)))

(define (exec-pop! vm)
  (stackpop! vm))

(define (exec-const! vm)
  (stackpush! vm (arg 1 vm)))

(define (exec-go! vm)
  (pcset! vm (arg 1 vm)))

(define (exec-tgo! vm)
  (if (true? (stackpop! vm))
      (pcset! vm (arg 1 vm))
      #f))

(define (exec-fgo! vm)
  (if (false? (stackpop! vm))
      (pcset! vm (arg 1 vm))
      #f))

(define (exec-save! vm)
  (stackpush! vm (save-state vm (arg 1 vm))))

(define (exec-return! vm)
  (let ((return-value #f)
        (saved-state #f))
    (begin (set! return-value (stackpop! vm))
           (set! saved-state (stackpop! vm))
           (restore-state! vm saved-state)
           (stackpush! vm return-value))))

(define (exec-call! vm)
  (envpop! vm)
  (vm-fn-set! vm (stackpop! vm))
  (vm-code-set! vm (fn-code (vm-fn vm)))
  (vm-env-set! vm (fn-env (vm-fn vm)))
  (pcset! vm 0)
  (vm-nargs-set! vm (arg 1 vm)))

(define (exec-args! vm)
  (let* ((expected-count (arg 1 vm))
         (supplied-count (vm-nargs vm)))
    (if (= expected-count supplied-count)
        (let ((vals (reverse (stacktake! vm expected-count))))
          (envpush! vm (list->vector vals)))
        (error "Wrong number of arguments supplied" (vm-instruction vm)))))

(define (exec-args.! vm)
  (let* ((expected-count (arg 1 vm))
         (supplied-count (vm-nargs vm)))
    (if (<= expected-count supplied-count)
        (let* ((restcount (- supplied-count expected-count))
               (restvals '())
               (reqvals '()))
          (begin (set! restvals (reverse (stacktake! vm restcount)))
                 (set! reqvals (reverse (stacktake! vm expected-count)))
                 (let* ((vals (append reqvals (list restvals))))
                   (envpush! vm (list->vector vals)))))
        (error "Too few arguments supplied" (vm-instruction vm)))))

(define (exec-fn! vm)
  (stackpush! vm (make-fn (fn-code (arg 1 vm))
                          (vm-env vm))))

(define (exec-prim! vm)
  (apply-prim! vm
               (arg 1 vm) ; the prim id
               (reverse (stacktake! vm (vm-nargs vm))))) ; the args

(define (exec-setcc! vm)
  (vm-stack-set! vm
                 (stacktop vm)))

(define (exec-cc! vm)
  (stackpush! vm
              (make-fn (vm-cc-code)
                       (list (vector (vm-stack vm))))))

(define (vmexec! vm)
  (let ((opc (opcode (vm-instruction vm))))
    (cond opc
          ((eqv? HALT opc)(exec-halt! vm))
          ((eqv? LREF opc)(exec-lref! vm))
          ((eqv? LSET opc)(exec-lset! vm))
          ((eqv? GREF opc)(exec-gref! vm))
          ((eqv? GSET opc)(exec-gset! vm))
          ((eqv? POP opc)(exec-pop! vm))
          ((eqv? CONST opc)(exec-const! vm))
          ((eqv? GO opc)(exec-go! vm))
          ((eqv? TGO opc)(exec-tgo! vm))
          ((eqv? FGO opc)(exec-fgo! vm))
          ((eqv? SAVE opc)(exec-save! vm))
          ((eqv? RETURN opc)(exec-return! vm))
          ((eqv? CALL opc)(exec-call! vm))
          ((eqv? ARGS opc)(exec-args! vm))
          ((eqv? ARGS. opc)(exec-args.! vm))
          ((eqv? FN opc)(exec-fn! vm))
          ((eqv? PRIM opc)(exec-prim! vm))
          ((eqv? SETCC opc)(exec-setcc! vm))
          ((eqv? CC opc)(exec-cc! vm))
          (else (error "Unknown opcode" opc)))))

(define (vmstep! vm)
  (vmfetch! vm)
  (incpc! vm)
  (vmexec! vm))

(define (vmrun vm)
  (let loop ()
    (if (vm-halted? vm)
        vm
        (begin (vmstep! vm)
               (loop)))))
