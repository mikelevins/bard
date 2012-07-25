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

(define (env:null) '())

(define (env:add-frame env fr)
  (cons fr env))

(define (vm:add-args-to-env! vm nargs)
  (let ((fr (make-vector nargs)))
    (let loop ((i (- nargs 1)))
      (if (< i 0)
          (vm:set-env! vm (env:add-frame (vm:env vm) fr))
          (begin
            (vector-set! fr i (vm:pop! vm))
            (loop (- i 1)))))))

(define (vm:add-varargs-to-env! vm required-count supplied-count)
  (let loop ((i (- supplied-count 1))
             (restarg '())
             (required-args '()))
    (if (>= i required-count)
        (loop (- i 1)(cons (vm:pop! vm) restarg) required-args)
        (if (>= i 0)
            (loop (- i 1) restarg (cons (vm:pop! vm) required-args))
            (vm:set-env! vm
                         (env:add-frame (vm:env vm)
                                        (apply vector
                                               (append required-args
                                                       (list restarg)))))))))

(define (vm:lvar-ref vm frame-index var-index)
  (let ((env (vm:env vm)))
    (vector-ref (list-ref env frame-index) var-index)))

(define (vm:lvar-set! vm frame-index var-index val)
  (let ((env (vm:env vm)))
   (vector-set! (list-ref env frame-index) var-index val)))

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

(define-type vm-return-record
  id: 28D56336-01B7-467F-B35B-88334FEEC25C
  constructor: %private-make-return-record
  (pc return-record-pc)
  (meth return-record-method)
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
  (method vm:method vm:set-method!)
  (code vm:code vm:set-code!)
  (pc vm:pc vm:set-pc!)
  (globals vm:globals)
  (env vm:env vm:set-env!)
  (stack vm:stack vm:set-stack!)
  (argcount vm:argcount vm:set-argcount!)
  (instruction vm:instruction vm:set-instruction!))

(define (vm:make-vm #!key
                 (method #f)
                 (pc 0)
                 (env (env:null))
                 (stack '())
                 (argcount 0)
                 (instruction #f))
  (%private-make-vm #f method (vm:method-code method) pc (vm:make-globals) env stack argcount instruction))

(define (vm:push! vm val)
  (vm:set-stack! vm (cons val (vm:stack vm))))

(define (vm:stack-ref vm n)(list-ref (vm:stack vm) n))

(define (vm:top vm)(vm:stack-ref vm 0))

(define (vm:pop! vm)
  (let ((out (vm:top vm)))
    (vm:set-stack! vm (cdr (vm:stack vm)))
    out))

(define (vm:swap! vm)
  (let ((old-stack (vm:stack vm)))
    (vm:set-stack! vm 
                   (cons (cadr old-stack)
                         (cons (car old-stack)
                               (cddr old-stack))))))

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
(defop 10 (SAVE addr) (vm:push! vm (vm:make-return-record pc: addr method: (vm:method vm) env: (vm:env vm))))

(defop 11 (RETURN) ;; return value is top of stack; return-record is second
  (let ((ret (vm:stack-ref vm 1))
         (meth (return-record-method ret)))
    (vm:set-method! vm meth)
    (vm:set-code! vm (vm:method-code meth))
    (vm:set-env! vm (return-record-environment ret))
    (vm:set-pc! vm (return-record-pc ret))
    ;; discard the return address and keep the value
    (vm:swap! vm)
    (vm:pop! vm)))

;;; function calls

(defop 12 (NARGS n) (vm:set-argcount! vm n))

(defop 13 (CALLJ argcount) 
  (begin
    (vm:pop! vm)
    (vm:set-method! (vm:pop! vm))
    (vm:set-code! (vm:method-code (vm:method vm)))
    (vm:set-env! vm (vm:method-environment (vm:method vm)))
    (vm:set-pc! vm 0)
    (vm:set-argcount! vm argcount)))

(defop 14 (ARGS nargs) 
  (let ((argcount (vm:argcount vm)))
    (if (= argcount nargs)
        (vm:add-args-to-env! vm nargs)
        (error (string-append "Wrong number of arguments; " 
                              (object->string nargs) " expected; "
                              (object->string argcount) " supplied.")))))

(defop 15 (ARGS. nargs) 
  (let ((argcount (vm:argcount vm)))
    (if (>= argcount nargs)
        (vm:add-varargs-to-env! vm nargs argcount)
        (error (string-append "Wrong number of arguments; " 
                              (object->string nargs) " or more expected; "
                              (object->string argcount) " supplied.")))))

(defop 16 (METH m)
  (vm:push! vm (vm:make-method code: (vm:method-code m)
                               env: (vm:env vm))))

(defop 17 (PRIM op)
  (let ((n-args (vm:argcount vm)))
    (let loop ((i 0)
               (args '()))
      (if (>= i n-args)
          (vm:push! vm (apply op args))
          (loop (+ i 1)(cons (vm:pop! vm) args))))))

;;; constant ops

(defop 18 (TRUE)(vm:push! vm #t))
(defop 19 (FALSE)(vm:push! vm #f))
(defop 20 (MINUSONE)(vm:push! vm -1))
(defop 21 (ZERO)(vm:push! vm 0))
(defop 22 (ONE)(vm:push! vm 1))
(defop 23 (TWO)(vm:push! vm 2))

;;; List ops

(define (vm:%empty-list) '())
(define (vm:%cons hd tl) (cons hd tl))
(define (vm:%car ls) (car ls))
(define (vm:%cdr ls) (cdr ls))

(defop 24 (NIL)(vm:push! vm (vm:%empty-list)))
(defop 25 (CONS) (let ((tl (vm:pop! vm))(hd (vm:pop! vm)))(vm:push! vm (vm:%cons hd tl))))
(defop 26 (CAR) (vm:push! vm (vm:%car (vm:pop! vm))))
(defop 27 (CDR) (vm:push! vm (vm:%cdr (vm:pop! vm))))

;;; EQ ops

(define (vm:%eq x y) (eq? x y))

(defop 28 (EQ) (vm:push! vm (vm:%eq (vm:pop! vm)(vm:pop! vm))))


;;; ---------------------------------------------------------------------
;;; displaying the vm
;;; ---------------------------------------------------------------------

(define (%instruction->string instr)
  (if instr
      (let* ((ilist (vector->list instr))
             (opstr (vm:opname (instr:opcode instr))))
        (object->string (cons opstr (cdr ilist))))
      ""))

(define (vm:show-instruction vm #!key (pad ""))
  (let* ((instr (vm:instruction vm)))
    (display pad)
    (display "instr: ")
    (if instr
        (display (%instruction->string instr))
        (display "[NONE]"))))

(define (%code->string code)
  (object->string (%disasm code)))

(define (vm:show-code vm #!key (pad ""))
  (let* ((code (vm:code vm))
         (len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (let ((instr (vector-ref code i)))
            (newline)
            (display pad)
            (display "  ")
            (display i)
            (display " ")
            (display (%instruction->string instr))
            (loop (+ i 1)))))))

(define (vm:show-env vm #!key (pad ""))
  (let* ((env (vm:env vm))
         (len (length env)))
    (display pad)
    (display "[")
    (if (> len 0)
        (let outer-loop ((i 0))
          (if (< i len)
              (let* ((fr (list-ref env i))
                     (l (vector-length fr)))
                (newline)
                (display pad)
                (display "  frame ")
                (display i)
                (display ": ")
                (let inner-loop ((j 0))
                  (if (< j l)
                      (let ((val (vector-ref fr j)))
                        (newline)
                        (display pad)
                        (display "    var ")
                        (display j)
                        (display ": ")
                        (display val)
                        (inner-loop (+ j 1)))))
                (outer-loop (+ i 1)))
              (newline))))
    (display pad)
    (display "]")
    (newline)))

(define (vm:show-pc vm)
  (let* ((pc (vm:pc vm)))
    (display "pc: ")
    (display (object->string pc))))

(define (%stack->string s)
  (object->string s))

(define (vm:show-stack vm)
  (let* ((stack (vm:stack vm)))
    (display "stack: ")
    (display (%stack->string stack))))

(define (vm:show-vm vm #!key 
                    (show-code #f)
                    (show-env #f)
                    (indent 0))
  (let ((pad (make-string indent #\space)))
    (if show-code 
        (begin
          (newline)
          (display pad)
          (display "code:")
          (vm:show-code vm pad: pad)
          (newline)))
    (if show-env 
        (begin
          (newline)
          (display pad)
          (display "env: ")
          (vm:show-env vm pad: pad)))
    (display pad)
    (vm:show-pc vm)
    (display " ")
    (vm:show-instruction vm)
    (display " halted: ")
    (display (object->string (vm:halted? vm)))
    (newline)
    (display pad)
    (vm:show-stack vm)
    (newline)))

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
  (newline)
  (display "step")
  (newline)
  (vm:set-instruction! vm (vector-ref (vm:code vm) (vm:pc vm)))
  (vm:set-pc! vm (+ 1 (vm:pc vm)))
  (let ((operation (vm:op (instr:opcode (vm:instruction vm)))))
    (operation vm))
  (vm:show-vm vm indent: 2))

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
  (display "-------------------")
  (newline)
  (display $bardvm-version-string)
  (newline)
  (display "-------------------")
  (newline)
  (vm:show-vm vm show-code: #t  show-env: #t)
  (newline)
  (display "start")
  (let loop ()
    (if (vm:halted? vm)
        (begin
          (display "stop")
          (newline)
          (newline)
          (display "-------------")
          (newline)
          (if (null? (vm:stack vm))
              (begin
                (display "No value returned.")
                (newline)(newline))
              (begin
                (display "Value = ")
                (display (vm:top vm))
                (newline)(newline)))
          (display "End state:")
          (newline)
          (vm:show-vm vm show-code: #t  show-env: #t indent: 2)
          (if (null? (vm:stack vm))
              '()
              (vm:top vm)))
        (begin
          (vm:step-show vm)
          (loop)))))

(define (vm:run-program code #!key (show #f))
  (let* ((f (vm:make-method code: code env: (env:null)))
         (vm (vm:make-vm method: (vm:make-method code: code env: (env:null)))))
    (if show
        (vm:run-show vm)
        (vm:run vm))
    (if (null? (vm:stack vm))
        #!void
        (vm:top vm))))

