;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard interpreter version string
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


(define $bardvm-version-string "Bard version 0.2.6")

;;; ---------------------------------------------------------------------
;;; global variables
;;; ---------------------------------------------------------------------

(define (vm:gref vm var)
  (table-ref (vm:globals vm) var $absent))

(define (vm:gset! vm var val)
  (table-set! (vm:globals vm) var val))

;;; ---------------------------------------------------------------------
;;; local environments
;;; ---------------------------------------------------------------------
;;; local environments are lists of vectors
;;; vm code accesses variables with a frame index and the index of the var
;;; in the frame

(define (vm:null-env) '())

(define (vm:add-frame env fr)
  (cons fr env))

(define (vm:make-frame args)
  (apply vector args))

(define (vm:lvar-ref env frame-index var-index)
  (vector-ref (list-ref env frame-index) var-index))

(define (vm:lvar-set! env frame-index var-index val)
  (vector-set! (list-ref env frame-index) var-index val))

;;; ---------------------------------------------------------------------
;;; vm instructions
;;; ---------------------------------------------------------------------

(define (opcode instr)(vector-ref instr 0))
(define (vm:frame-index instr)(vector-ref instr 1))
(define (vm:var-index instr)(vector-ref instr 2))
(define (vm:gvarname instr)(vector-ref instr 1))
(define (vm:arg1 instr)(vector-ref instr 1))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-type vmfunction
  id: DC746B5B-F06C-4735-BAF2-8EEEFA5355CE
  constructor: %private-make-vmfunction
  (code vm:function-code)
  (env vm:function-environment))

(define (vm:make-function #!key code env)
  (%private-make-vmfunction code env))

(define (vm:as-code args)
  (apply vector (map (lambda (a)(apply vector a)) args)))

;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define-type vm
  id: 85820E73-753A-4EF8-B575-5A330200197A
  constructor: %private-make-vm
  (halted vm:halted? vm:set-halted!)
  (fun vm:fun vm:set-fun!)
  (code vm:code vm:set-code!)
  (pc vm:pc vm:set-pc!)
  (globals vm:globals)
  (env vm:env vm:set-env!)
  (stack vm:stack vm:set-stack!)
  (argcount vm:argcount vm:set-argcount!)
  (instruction vm:instruction vm:set-instruction!))

(define (vm:make-vm #!key
                 (fun #f)
                 (pc 0)
                 (env '())
                 (stack '())
                 (argcount 0)
                 (instruction #f))
  (%private-make-vm #f fun (vm:function-code fun) pc (make-table test: eqv?) env stack argcount instruction))

(define (vm:push! vm val)
  (vm:set-stack (cons val (vm:stack vm))))

(define (vm:stacknth vm n)(list-ref (vm:stack vm) n))

(define (vm:top vm)(vm:stacknth vm 0))

(define (vm:pop! vm)
  (let ((out (vm:top vm)))
    (vm:set-stack (cdr (vm:stack vm)))
    out))

(define $absent (cons '() '()))

(define (vm:step vm)
  (vm:set-instruction! vm (vector-ref (vm:code vm) (vm:pc vm)))
  (vm:set-pc! vm (+ 1 (vm:pc vm)))

  (let ((op (opcode (vm:instruction vm))))

    (case op

      ;; Variable/stack manipulation instructions:
      ((LVAR)   (vm:push! vm (vm:lvar-ref (vm:env vm)
                                          (vm:frame-index (vm:instruction vm))
                                          (vm:var-index (vm:instruction vm)))))

      ((LSET)   (vm:lvar-set! vm 
                              (vm:frame-index (vm:instruction vm))
                              (vm:var-index (vm:instruction vm))
                              (vm:top vm)))

      ((GVAR)   (vm:push! vm (vm:gref vm (vm:gvarname (vm:instruction vm)))))
      ((GSET)   (vm:gset! vm (vm:gvarname (vm:instruction vm)) (vm:top vm)))
      ((POP)    (vm:pop! vm))
      ((CONST)  (vm:push! vm (vm:arg1 (vm:instruction vm))))
      
      ;; Branching instructions:
      ((JUMP)   (vm:set-pc! vm (vm:arg1 (vm:instruction vm))))
      ((FJUMP)  (if (false? (vm:pop! vm)) (vm:set-pc! vm (vm:arg1 (vm:instruction vm)))))
      ((TJUMP)  (if (true? (vm:pop! vm)) (vm:set-pc! vm (vm:arg1 (vm:instruction vm)))))
      
      ;; Function call/return instructions:
      ((SAVE)   (vm:push! vm (vm:make-return-address pc: (vm:arg1 (vm:instruction vm))
                                                     fn: (vm:fun vm)
                                                     env: (vm:env vm))))

      ((RETURN) ;; return value is top of stack; ret-addr is second
       (vm:set-fun! vm (vm:return-address-function (vm:stacknth vm 1)))
       (vm:set-code! vm (vm:function-code (vm:fun vm)))
       (vm:set-env! vm (vm:return-address-environment (vm:stacknth vm 1)))
       (vm:set-pc! vm (vm:return-address-pc (vm:stacknth vm 1)))
       ;; Get rid of the ret-addr, but keep the value
       (vm:set-stack! (cons (vm:top vm)(cddr (vm:stack vm)))))

      ((CALLJ)  (vm:set-env! vm (cdr (vm:env vm)))                 ; discard the top frame
       (vm:set-fun! vm (vm:pop vm))
       (vm:set-code! vm (vm:function-code (vm:fun vm)))
       (vm:set-env! vm (vm:function-environment (vm:fun vm)))
       (vm:set-pc! vm 0)
       (vm:set-argcount! vm (vm:arg1 (vm:instruction vm))))

      ((ARGS)   (let ((n-args (vm:arg1 (vm:instruction vm)))
                      (argcount (vm:argcount vm)))
                  (if (= argcount n-args)
                      (let loop ((i 0)
                                 (args '()))
                        (if (>= i n-args)
                            (vm:set-env! vm (vm:add-frame (vm:env vm) (vm:make-frame args)))
                            (loop (+ i 1)
                                  (cons (vm:pop vm) args))))
                      (error (string-append "Wrong number of arguments; " 
                                            (object->string n-args)
                                            " expected; "
                                            (object->string argcount)
                                            " supplied.")))))

      ((ARGS.)   (let ((n-args (vm:arg1 (vm:instruction vm)))
                       (argcount (vm:argcount vm)))
                   (if (>= argcount n-args)
                       (let loop ((i 0)
                                  (args '())
                                  (rest '()))
                         (if (>= i argcount)
                             (vm:set-env! vm (vm:add-frame (vm:env vm) (vm:make-frame (append args (list rest)))))
                             (if (>= i (- argcount n-args))
                                 (loop (+ i 1)
                                       args
                                       (cons (vm:pop vm) rest))
                                 (loop (+ i 1)
                                       (cons (vm:pop vm) args)
                                       rest))))
                       (error (string-append "Wrong number of arguments; " 
                                             (object->string n-args)
                                             " or more expected; "
                                             (object->string argcount)
                                             " supplied.")))))

      ((FN)     (vm:push! vm (vm:make-function code: (vm:function-code (vm:arg1 (vm:instruction vm)))
                                               env: (vm:env vm))))

      ((PRIM)   (let ((n-args (vm:argcount vm)))
                  (let loop ((i 0)
                             (args '()))
                    (if (>= i n-args)
                        (vm:push! vm (apply (vm:arg1 (vm:instruction vm)) args))
                        (loop (+ i 1)(cons (vm:pop vm) args))))))
      
      ;; Continuation instructions:
      ((SET-CC) (vm:set-stack! (vm:top vm)))
      
      ((CC)     (vm:push! vm (vm:make-function code: (vm:as-code '((ARGS 1) (LVAR 1 0 ";" stack) (SET-CC)
                                                                   (LVAR 0 0) (RETURN)))
                                               env: (list (vector (vm:stack vm))))))
      
      ;; Nullary operations:
      ((NEWLINE)  (vm:push! vm (exec (opcode (vm:instruction vm)))))
      
      ;; Unary operations:
      ((CAR CDR CADR NOT RANDOM) 
       (vm:push! vm (exec (opcode (vm:instruction vm))
                          (vm:pop vm))))
      
      ;; Constants:
      ((T NIL -1 0 1 2)
       (vm:push! (vm:const-op (opcode (vm:instruction vm)))))
      
      ;; Other:
      ((HALT) (vm:set-halted! vm #t))
      
      (else (error (string-append "Unknown opcode: " (object->string op)))))))

