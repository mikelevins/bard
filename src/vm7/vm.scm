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

(define $bard-vm-version "0.3.0")

;;; ---------------------------------------------------------------------
;;; conditions
;;; ---------------------------------------------------------------------

(define-type condition
  extender: define-condition
  message)

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------


;;; ---------------------------------------------------------------------
;;; utility predicates
;;; ---------------------------------------------------------------------

(define nothing? null?)
(define (true? x) (eqv? x #t))
(define (false? x) (eqv? x #f))

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(define (instr opc . args)(list opc args))
(define (op inst)(car inst))
(define (arg1 inst)(list-ref inst 1))
(define (arg2 inst)(list-ref inst 2))
(define (arg3 inst)(list-ref inst 3))
(define (args inst)(cdr inst))

(define (set-op! inst v)(set-car! inst v))
(define (set-args! inst args)(set-cdr! inst args))

;;; ---------------------------------------------------------------------
;;; vm ops
;;; ---------------------------------------------------------------------

(define $max-opcode 25)

(define $optable (make-vector (+ $max-opcode 1) #f))
(define $opnames-table (make-vector (+ $max-opcode 1) #f))

(define-macro (defop opcode opname opfn)
  `(begin
     (define ,opname ,opcode)
     (vector-set! $optable ,opcode ,opfn)
     (vector-set! $opnames-table ,opcode ',opname)))

(define (opcode->op opc)(vector-ref $optable opc))

(define (op->opcode op)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? op (vector-ref $optable i))
            i
            (loop (+ 1 i)))
        #f)))

(define (op->opname op)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? op (vector-ref $optable i))
            (vector-ref $opnames-table i)
            (loop (+ 1 i)))
        #f)))

(define (opname->opcode opnm)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? opnm (vector-ref $opnames-table i))
            i
            (loop (+ 1 i)))
        #f)))

;;; ---------------------------------------------------------------------
;;; globals
;;; ---------------------------------------------------------------------

(define (make-globals)(make-table test: eqv?))
(define (mref globals var)(table-ref globals var $undefined))
(define (mset! globals var val)(table-set! globals var val))

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (null-environment) '())

(define (add-binding env var val)(cons (cons var val) env))
(define (find-binding env var)(assq var env))

(define (lref env var)
  (let ((binding (find-binding env var)))
    (if binding
        (cdr binding)
        $undefined)))

(define (lset! vm env var val)
  (let ((binding (find-binding env var)))
    (if binding
        (set-cdr! binding val)
        (vm-env-set! vm (add-binding env var val)))))

;;; ---------------------------------------------------------------------
;;; prims
;;; ---------------------------------------------------------------------

(define-type vmprim
  name
  nargs
  fn)

(define $max-prim 24)

(define $primtable (make-vector (+ $max-prim 1) #f))
(define $prim-names-table (make-vector (+ $max-prim 1) #f))

(define-macro (defprim pcode pname nargs pfn)
  `(begin
     (define ,pname ,pcode)
     (vector-set! $primtable ,pcode (make-vmprim ',pname ,nargs ,pfn))
     (vector-set! $prim-names-table ,pcode ',pname)))

(define (pcode->prim pc)(vector-ref $primtable pc))

(define (prim->primname p)
  (vector-ref $prim-names-table p))

(define (apply-prim vm p args)
  (apply (vmprim-fn p) args))

;;; the prims

(defprim 1 PRIM+ 2 (lambda (x y)(+ x y)))

;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define-type vm
  env
  globals
  vals
  code
  pc
  instr
  stack)

;;; ---------------------------------------------------------------------
;;; base data types
;;; ---------------------------------------------------------------------

;;; data types to add:
;;; map
;;; method
;;; function

;;; ---------------------------------------------------------------------
;;; aux machine functions
;;; ---------------------------------------------------------------------

(define (push-val! vm v)
  (vm-vals-set! vm (cons v (vm-vals vm))))

(define (pop-val! vm)
  (let ((val (car (vm-vals vm))))
    (vm-vals-set! vm (cdr (vm-vals vm)))
    val))

(define (clear-vals! vm)
  (vm-vals-set! vm '()))

(define (vm-require-argcount vm argcount)
  (or (= argcount (length (vm-vals vm)))
      (error "Wrong number of arguments")))

(define (slot-ref obj sname) #f)
(define (slot-set! obj sname val) #f)
(define (make-method arglist mbody env) #f)
(define (makefn) #f)
(define (function-add-method! fn sig meth) #f)
(define (best-applicable-method! fn sig) #f)
(define (take-vals vm n) #f)
(define (extend-env! vm vars vals) #f)
(define (method-code method) #f)
(define (push-state! vm) #f)
(define (pop-state! vm) #f)

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

;;; halting the vm

(defop 0 HALT (lambda (vm) #f)) ; dummy op; gets replaced at vm init time

;;; variables and values

(defop  1 LVAR (lambda (vm var) (push-val! vm (lref (vm-env vm) var))))
(defop  2 LSET (lambda (vm var) (lset! vm (vm-env vm) var (pop-val! vm))))
(defop  3 MVAR (lambda (vm var) (push-val! vm(mref (vm-globals vm) var))))
(defop  4 MSET (lambda (vm var) (mset! (vm-globals vm) var (pop-val! vm))))
(defop  5 SLOT (lambda (vm obj sname) (push-val! vm (slot-ref obj sname))))
(defop  6 SSET (lambda (vm obj sname) (slot-set! obj sname (pop-val! vm))))
(defop  7 POP  (lambda (vm) (pop-val! vm)))

;;; constants

(defop  8 CONST   (lambda (vm k) (push-val! vm k)))
(defop  9 NOTHING (lambda (vm) (push-val! vm $nothing)))
(defop 10 TRUE    (lambda (vm) (push-val! vm $true)))
(defop 11 FALSE   (lambda (vm) (push-val! vm $false)))
(defop 12 ZERO    (lambda (vm) (push-val! vm 0)))
(defop 13 ONE     (lambda (vm) (push-val! vm 1)))
(defop 14 TWO     (lambda (vm) (push-val! vm 2)))
(defop 15 NEG1    (lambda (vm) (push-val! vm -1)))

;;; functions

(defop 16 METH (lambda (vm)
                 (let ((mbody (pop-val! vm))
                       (arglist (pop-val! vm)))
                   (push-val! vm (make-method arglist mbody (vm-env vm))))))

(defop 17 FN (lambda (vm) (push-val! vm (makefn))))

(defop 18 DEFM (lambda (vm)
                 (let ((body (pop-val! vm))
                       (sig (pop-val! vm)))
                   (push-val! (make-method sig body (vm-env vm))))))

(defop 19 DISP (lambda (vm)
                 (let ((args (pop-val! vm))
                       (f (pop-val! vm)))
                   (push-val! vm args)
                   (push-val! (best-applicable-method f args)))))

;;; function-calling and control

(defop 20 IF (lambda (vm thend elsed) 
               (if (pop-val! vm)
                   (vm-pc-set! vm thend)
                   (vm-pc-set! vm elsed))))

(defop 21 ARGS (lambda (vm) 
                 (let* ((method (top-val vm))
                        (params (msig method))
                        (vals (take-vals vm (length params))))
                   (extend-env! vm params vals))))

(defop 22 APPLY (lambda (vm) 
                  (vm-code-set! vm (method-code (pop-val! vm)))
                  (vm-pc-set! vm 0)))

(defop 23 SAVE (lambda (vm) (push-state! vm)))

(defop 24 RESTORE (lambda (vm) (pop-state! vm)))

(defop 25 PRIM (lambda (vm pcode) 
                 (let* ((p (pcode->prim pcode))
                        (argcount (vmprim-nargs p))
                        (args (vm-vals vm)))
                   (vm-require-argcount vm argcount)
                   (clear-vals! vm)
                   (push-val! vm (apply-prim vm p args)))))

;;; ---------------------------------------------------------------------
;;;  saving and loading compiled code
;;; ---------------------------------------------------------------------

(define (link-instruction! vm instr)
  (set-op! instr (opcode->op (op instr)))
  (set-args! instr (cons vm (args instr)))
  instr)

(define (vmlink vm code)
  (let ((len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (link-instruction! vm (vector-ref code i))
            (loop (+ i 1)))
          code))))

(define $bard-vmo-sentinel "bvmo")

(define-type vmo
  constructor: %private-make-vmo
  sentinel
  version
  code)

(define (make-vmo vm code)
  (%private-make-vmo $bard-vmo-sentinel $bard-vm-version vm))

(define (code->vmo vm code)
  (let ((vmo (make-vmo vm code)))
    (object->u8vector vmo)))

(define (vmo->code vmodata)
  (let ((vmo (u8vector->object vmodata)))
    (vmo-code vmo)))

;;; ---------------------------------------------------------------------
;;; vm images
;;; ---------------------------------------------------------------------

(define $bard-vm-sentinel "bdvm")

(define-type vmimage
  constructor: %private-make-vmimage
  sentinel
  version
  vm)

(define (make-vmimage vm)
  (%private-make-vmimage $bard-vm-sentinel $bard-vm-version vm))

(define (vm->image vm)
  (let ((img (make-vmimage vm)))
    (object->u8vector img)))

(define (image->vm imgdata)
  (let ((img (u8vector->object imgdata)))
    (vmimage-vm img)))

;;; ---------------------------------------------------------------------
;;; running
;;; ---------------------------------------------------------------------

(define (vmfetch vm)
  (vm-instr-set! vm (vector-ref (vm-code vm)(vm-pc vm))))

(define (vminc vm)
  (vm-pc-set! vm (+ 1 (vm-pc vm))))

(define (vmexec op args)
  (apply op args))

(define (step vm)
  (vmfetch vm)
  (vminc vm)
  (vmexec (op (vm-instr vm))(args (vm-instr vm))))

;;; ---------------------------------------------------------------------
;;; tests
;;; ---------------------------------------------------------------------

(define (print-instruction instr)
  (let* ((op (car instr))
         (nm (op->opname op))
         (args (cddr instr)))
    (display nm)
    (if (eq? nm 'PRIM)
        (let ((pname (prim->primname (car args))))
          (display " ")(display pname))
        (for-each (lambda (arg)(display " ")(display arg))
                  args))))

(define (print-code vm code)
  (let ((len (vector-length code))
        (pc (vm-pc vm)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (newline)
            (if (= i pc)
                (display ">")
                (display " "))
            (display (object->string i))(display ". ")
            (print-instruction (vector-ref code i))
            (loop (+ i 1)))
          (newline)))))

(define (print-globals vm)
  (table-for-each (lambda (k v)
                    (begin
                      (newline)(display "  ")
                      (display (object->string k))(display ": ")
                      (display (object->string v))))
                  (vm-globals vm)))

(define (vmprint vm)
  (newline)
  (display "pc: ")(display (vm-pc vm))(newline)
  (display "instr: ")(print-instruction (vm-instr vm))(newline)
  (display "vals: ")(display (vm-vals vm))(newline)
  (display "stack: ")(display (vm-stack vm))(newline)
  (display "env: ")(display (vm-env vm))(newline)
  (display "globals: ")(print-globals vm)(newline)
  (display "code: ")(print-code vm (vm-code vm))(newline)
  (newline))

(define (testvm code)
  (let ((m (make-vm (null-environment)
                    (make-globals)
                    '()
                    #f
                    0
                    #f
                    '())))
    (vm-code-set! m (vmlink m code))
    m))

(define (teststep vm)
  (step vm)
  (vmprint vm))

(define (@ . args)(apply vector args))
(define ($ . args)(apply list args))

;;; (define $vm (testvm (@ ($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST 5)($ LSET 'x)($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST 5)($ LSET 'x)($ LVAR 'x)($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST 2)($ CONST 3)($ PRIM PRIM+)($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST $false)($ IF 2 3)($ CONST 'yes)($ CONST 'no)($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST $true)($ IF 2 3)($ CONST 'yes)($ CONST 'no)($ HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (@ ($ CONST $true)($ MSET 'x)($ CONST 0)($ MVAR 'x)($ HALT))))
;;; (teststep $vm)
