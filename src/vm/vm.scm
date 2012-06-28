;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          version.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm implementation
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "asm.scm")

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(define (false? x)(not x))
(define (true? x)(not (false? x)))

;;; ---------------------------------------------------------------------
;;; global variables
;;; ---------------------------------------------------------------------

(define (vm:make-globals)(make-table test: eq?))

(define $absent (cons 'absent '()))

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

(define (instr:opcode instr)(vector-ref instr 0))
(define (instr:arg instr n)(vector-ref instr n))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-type vmmethod
  id: DC746B5B-F06C-4735-BAF2-8EEEFA5355CE
  constructor: %private-make-vmmethod
  (code vm:method-code)
  (env vm:method-environment))

(define (vm:make-method #!key code env)
  (%private-make-vmmethod code env))

(define (vm:as-code args)
  (apply vector (map (lambda (a)(apply vector a)) args)))

;;; ---------------------------------------------------------------------
;;; return addresses
;;; ---------------------------------------------------------------------

(define-type vm return-record
  id: 28D56336-01B7-467F-B35B-88334FEEC25C
  constructor: %private-make-return-record
  (pc return-record-pc)
  (method return-record-method)
  (env return-record-environment))

(define (vm:make-return-record #!key
                               pc method env)
  (%private-make-return-record pc method env))

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
  (%private-make-vm #f fun (vm:method-code fun) pc (vm:make-globals) env stack argcount instruction))

(define (vm:push! vm val)
  (vm:set-stack! vm (cons val (vm:stack vm))))

(define (vm:stacknth vm n)(list-ref (vm:stack vm) n))

(define (vm:top vm)(vm:stacknth vm 0))

(define (vm:pop! vm)
  (let ((out (vm:top vm)))
    (vm:set-stack! vm (cdr (vm:stack vm)))
    out))

(define (vm:swap! vm)
  (let ((old-top (vm:pop vm))
        (old-next (vm:pop vm)))
    (vm:push! vm old-top)
    (vm:push! vm old-next)
    old-next))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

(define $vm-operations (make-vector 255))
(define $vm-operation-names (make-vector 255))

(define (vm:op n)(vector-ref $vm-operations n))
(define (vm:opname n)(vector-ref $vm-operation-names n))

(define-macro (operation proto body)
  (let* ((nm (car proto))
         (args (cdr proto))
         (bindings (let loop ((args args)
                              (i 0)
                              (blist '()))
                     (if (null? args)
                         (reverse blist)
                         (loop (cdr args)
                               (+ i 1)
                               (cons `(,(car args) (instr:arg (vm:instruction vm) (+ 1 ,i)))
                                     blist)))))
         (fn (gensym)))
    `(lambda (vm)(let ,bindings ,@body))))

(define-macro (defop num proto . body)
  `(begin
     (define ,(car proto) ,num)
     (vector-set! $vm-operations ,num (operation ,proto ,body))
     (vector-set! $vm-operation-names ,num ',(car proto))))

(defop  0 (HALT)(vm:set-halted! vm #t))
(defop  1 (LVAR frame var) (vm:push! vm (vm:lvar-ref vm frame var)))
(defop  2 (LSET frame var) (vm:lvar-set! vm frame var (vm:top vm)))
(defop  3 (GVAR vname) (vm:push! vm (vm:gref vm vname)))
(defop  4 (GSET vname) (vm:gset! vm vname (vm:top vm)))
(defop  5 (POP) (vm:pop! vm))
(defop  6 (CONST c) (vm:push! vm c))
(defop  7 (JUMP dest) (vm:set-pc! vm dest))
(defop  8 (FJUMP dest) (if (false? (vm:pop! vm)) (vm:set-pc! vm dest)))
(defop  9 (TJUMP dest) (if (true? (vm:pop! vm)) (vm:set-pc! vm dest)))
(defop 10 (SAVE dest) (vm:push! vm (vm:make-return-record pc: dest method: (vm:fun vm) env: (vm:env vm))))

(defop 11 (RETURN) ;; return value is top of stack; return-record is second
  (begin 
    (vm:swap! vm) ;; swap so return address is on top
    (vm:set-fun! vm (vm:return-record-method (vm:top vm)))
    (vm:set-code! vm (vm:method-code (vm:fun vm)))
    (vm:set-env! vm (vm:return-record-environment (vm:top vm)))
    (vm:set-pc! vm (vm:return-record-pc (vm:top vm)))
    ;; discard return address
    (vm:pop! vm)))

;;; function calls

(defop 12 (CALLJ argcount) 
  (begin
    (vm:set-env! vm (cdr (vm:env vm)))                 ; discard the top frame
    (vm:set-fun! vm (vm:pop vm))
    (vm:set-code! vm (vm:method-code (vm:fun vm)))
    (vm:set-env! vm (vm:method-environment (vm:fun vm)))
    (vm:set-pc! vm 0)
    (vm:set-argcount! vm argcount)))

(defop 13 (ARGS nargs) 
  (let ((argcount (vm:argcount vm)))
    (if (= argcount nargs)
        (let loop ((i 0)(args '()))
          (if (>= i nargs)
              (vm:set-env! vm (vm:add-frame (vm:env vm) (vm:make-frame args)))
              (loop (+ i 1)(cons (vm:pop vm) args))))
        (error (string-append "Wrong number of arguments; " 
                              (object->string nargs) " expected; "
                              (object->string argcount) " supplied.")))))

(defop 14 (ARGS. n-args) 
  (let ((argcount (vm:argcount vm)))
    (if (>= argcount n-args)
        (let loop ((i 0) (args '()) (rest '()))
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
                              (object->string n-args) " or more expected; "
                              (object->string argcount) " supplied.")))))

(defop 15 (METH fun)
  (vm:push! vm (vm:make-method code: (vm:method-code fun)
                               env: (vm:env vm))))

(defop 16 (PRIM op)
  (let ((n-args (vm:argcount vm)))
    (let loop ((i 0)
               (args '()))
      (if (>= i n-args)
          (vm:push! vm (apply op args))
          (loop (+ i 1)(cons (vm:pop vm) args))))))

;;; constant ops

(defop 17 (TRUE)(vm:push! vm #t))
(defop 18 (FALSE)(vm:push! vm #f))
(defop 19 (MINUSONE)(vm:push! vm -1))
(defop 20 (ZERO)(vm:push! vm 0))
(defop 21 (ONE)(vm:push! vm 1))
(defop 22 (TWO)(vm:push! vm 2))

;;; List ops

(define (vm:%empty-list) '())
(define (vm:%cons hd tl) (cons hd tl))
(define (vm:%cdr ls) (cdr ls))

(defop 23 (NIL)(vm:push! vm (vm:%empty-list)))
(defop 24 (CONS hd tl) (vm:push! vm (vm:%cons hd tl)))
(defop 25 (CAR ls) (vm:push! vm (vm:%car ls)))
(defop 26 (CDR ls) (vm:push! vm (vm:%cdr ls)))

;;; EQ ops

(defop 27 (EQ x y) (vm:push! vm (vm:%eq x y)))

;;; ---------------------------------------------------------------------
;;; displaying the vm
;;; ---------------------------------------------------------------------

(define (%instruction->string instr)
  (if instr
      (let* ((ilist (vector->list instr))
             (opstr (vm:opname (instr:opcode instr))))
        (object->string (cons opstr (cdr ilist))))
      ""))

(define (vm:show-instruction vm)
  (let* ((instr (vm:instruction vm)))
    (newline)
    (display "  instr: ")
    (display (%instruction->string instr))))

(define (%code->string code)
  (object->string (%disasm code)))

(define (vm:show-code vm)
  (let ((code (vm:code vm)))
    (newline)
    (display "   code: ")
    (display (%code->string code))))

(define (vm:show-pc vm)
  (let* ((pc (vm:pc vm)))
    (newline)
    (display "  pc: ")
    (display (object->string pc))))

(define (%stack->string s)
  (object->string s))

(define (vm:show-stack vm)
  (let* ((stack (vm:stack vm)))
    (newline)
    (display "  stack: ")
    (display (%stack->string stack))))

(define (vm:show-vm vm)
  (newline)
  (display " halted: ")
  (display (object->string (vm:halted? vm)))
  (vm:show-instruction vm)
  (vm:show-pc vm)
  (vm:show-stack vm)
  (vm:show-code vm)
  (newline)
  (newline))

;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

(define-macro (vm:prog . instrs)
  (if (null? instrs)
      '(list)
      `(cons (list ,@(car instrs)) (vm:prog . ,(cdr instrs)))))

(define (vm:step vm)
  (vm:set-instruction! vm (vector-ref (vm:code vm) (vm:pc vm)))
  (vm:set-pc! vm (+ 1 (vm:pc vm)))
  (let ((operation (vm:op (instr:opcode (vm:instruction vm)))))
    (operation vm)))

(define (vm:step-show vm)
  (display "step:")
  (vm:show-pc vm)
  (vm:show-stack vm)
  (vm:set-instruction! vm (vector-ref (vm:code vm) (vm:pc vm)))
  (vm:show-instruction vm)
  (vm:set-pc! vm (+ 1 (vm:pc vm)))
  (let ((operation (vm:op (instr:opcode (vm:instruction vm)))))
    (operation vm))
  (newline)
  (newline))

(define (vm:run vm)
  (let loop ()
    (if (vm:halted? vm)
        (if (null? (vm:stack vm))
            '()
            (vm:top vm))
        (begin
          (vm:step vm)
          (loop)))))

(define (vm:run-show vm)
  (newline)
  (display "start")
  (vm:show-vm vm)
  (let loop ()
    (if (vm:halted? vm)
        (begin
          (display "stop")
          (vm:show-vm vm)
          (if (null? (vm:stack vm))
              '()
              (vm:top vm)))
        (begin
          (vm:step-show vm)
          (loop)))))

(define (vm:run-program code #!key (show #f))
  (let* ((f (vm:make-method code: code env: '()))
         (m (vm:make-vm fun: f)))
    (if show
        (vm:run-show m)
        (vm:run m))
    (if (null? (vm:stack m))
        #!void
        (vm:top m))))

#| tests

(vm:run-program (%asm ((HALT))) show: #t)
(vm:run-program (%asm ((CONST 5)(HALT))) show: #t)
(vm:run-program (%asm ((CONST 5)(GSET 'x)(POP)(HALT))) show: #t)
(vm:run-program (%asm ((CONST 5)(GSET 'x)(POP)(GVAR 'x)(HALT))) show: #t)
(vm:run-program (%asm ((JUMP 3)(TRUE)(TJUMP 5)(FALSE)(FJUMP 1)(HALT))) show: #t)

|#


