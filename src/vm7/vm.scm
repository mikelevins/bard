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

(define $undefined #!unbound)
(define $nothing '())
(define $true #t)
(define $false #f)

;;; ---------------------------------------------------------------------
;;; primitive predicates
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

(define $max-opcode 24)

(define $optable (make-vector (+ $max-opcode 1) #f))
(define $opnames-table (make-vector (+ $max-opcode 1) #f))

(define-macro (defop opcode opname opfn)
  `(begin
     (define ,opname ,opcode)
     (vector-set! $optable ,opcode ,opfn)
     (vector-set! $opnames-table ,opcode ',opname)))

(define (opcode->op opc)(vector-ref $optable opc))

(define (op->opname op)
  (let loop ((i 0))
    (if (<= i $max-opcode)
        (if (eq? op (vector-ref $optable i))
            (vector-ref $opnames-table i)
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

(define (push-val! vm v)
  (vm-vals-set! vm (cons v (vm-vals vm))))

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

;;; halting the vm

(defop 0 HALT (lambda (vm) #f)) ; dummy op; gets replaced at vm init time

;;; variables

(defop 1 LVAR (lambda (vm var) (push-val! vm (lref (vm-env vm) var))))
(defop 2 LSET (lambda (vm var val) (lset! vm (vm-env vm) var val)))
(defop 3 MVAR (lambda (vm var) (mref (vm-globals vm) var)))
(defop 4 MSET (lambda (vm var val) (lset! (vm-globals vm) var val)))
(defop 5 SLOT (lambda (vm obj sname) (slot-ref obj sname)))
(defop 6 SSET (lambda (vm obj sname val) (slot-set! obj sname val)))

;;; constants

(defop  7 CONST   (lambda (vm k) (push-val! vm k)))
(defop  8 NOTHING (lambda (vm) (push-val! vm $nothing)))
(defop  9 TRUE    (lambda (vm) (push-val! vm $true)))
(defop 10 FALSE   (lambda (vm) (push-val! vm $false)))
(defop 11 ZERO    (lambda (vm) (push-val! vm 0)))
(defop 12 ONE     (lambda (vm) (push-val! vm 1)))
(defop 13 TWO     (lambda (vm) (push-val! vm 2)))
(defop 14 NEG1    (lambda (vm) (push-val! vm -1)))

;;; functions

(defop 15 METH (lambda (vm arglist mbody) (push-val! vm (make-method arglist mbody (vm-env vm)))))
(defop 16 FN (lambda (vm) (push-val! vm (makefn))))

(defop 17 DEFM (lambda (vm sig m)
                 (let ((fn (pop-val! vm)))
                   (function-add-method! fn sig m)
                   (push-val! vm fn))))

(defop 18 DISP (lambda (vm fn sig) (push-val! vm (best-applicable-method fn sig))))

;;; function-calling and control

(defop 19 IF (lambda (vm test thenc elsec) 
               (if test
                   (vm-code-set! thenc)
                   (vm-code-set! elsec))))

(defop 20 ARGS (lambda (vm) 
                 (let* ((method (top-val vm))
                        (params (msig method))
                        (vals (take-vals vm (length params))))
                   (extend-env! vm params vals))))

(defop 21 APPLY (lambda (vm) 
                  (vm-code-set! vm (method-code (pop-val! vm)))
                  (vm-pc-set! vm 0)))

(defop 22 SAVE (lambda (vm) (push-state! vm)))

(defop 23 RESTORE (lambda (vm) (pop-state! vm)))

(defop 24 PRIM (lambda (vm p) 
                 (let* ((argcount (prim-nargs p)))
                   (vm-require-argcount vm argcount)
                   (vm-vals-set! vm (apply-prim vm p (vm-vals vm))))))

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
  (let ((nm (op->opname (car instr)))
        (args (cddr instr)))
    (display nm)
    (for-each (lambda (arg)(display " ")(display arg))
              args)))

(define (print-code code)
  (let ((len (vector-length code)))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (newline)
            (display " ")(display (object->string i))(display ". ")
            (print-instruction (vector-ref code i))
            (loop (+ i 1)))
          (newline)))))

(define (vmprint vm)
  (newline)
  (display "pc: ")(display (vm-pc vm))(newline)
  (display "instr: ")(print-instruction (vm-instr vm))(newline)
  (display "vals: ")(display (vm-vals vm))(newline)
  (display "stack: ")(display (vm-stack vm))(newline)
  (display "env: ")(display (vm-env vm))(newline)
  (display "code: ")(print-code (vm-code vm))(newline)
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

;;; (define $vm (testvm (vector (list HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (vector (list LSET 'x 5)(list HALT))))
;;; (teststep $vm)
;;;
;;; (define $vm (testvm (vector (list LSET 'x 5)(list LVAR 'x)(list HALT))))
;;; (teststep $vm)
