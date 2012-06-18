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
  (vm:set-stack! vm (cons val (vm:stack vm))))

(define (vm:stacknth vm n)(list-ref (vm:stack vm) n))

(define (vm:top vm)(vm:stacknth vm 0))

(define (vm:pop! vm)
  (let ((out (vm:top vm)))
    (vm:set-stack (cdr (vm:stack vm)))
    out))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

(define $vm-operations (make-vector 255))
(define $vm-operation-names (make-vector 255))

(define-macro (defop nm num fn)
  `(begin
     (define ,nm ,num)
     (vector-set! $vm-operations ,num ,fn)
     (vector-set! $vm-operation-names ,num ,nm)))

(define (vm:op n)(vector-ref $vm-operations n))
(define (vm:opname n)(vector-ref $vm-operation-names n))

(defop LVAR  1 
  (lambda (vm)
    (vm:push! vm (vm:lvar-ref (vm:env vm)
                              (vm:frame-index (vm:instruction vm))
                              (vm:var-index (vm:instruction vm))))))

(defop LSET  2
  (lambda (vm)
    (vm:lvar-set! vm 
                  (vm:frame-index (vm:instruction vm))
                  (vm:var-index (vm:instruction vm))
                  (vm:top vm))))

(defop GVAR  3
  (lambda (vm)
    (vm:push! vm (vm:gref vm (vm:gvarname (vm:instruction vm))))))

(defop GSET  4
  (lambda (vm)
    (vm:gset! vm (vm:gvarname (vm:instruction vm)) (vm:top vm))))

(defop POP   5 (lambda (vm)(vm:pop! vm)))

(defop CONST 6 (lambda (vm) (vm:push! vm (vm:arg1 (vm:instruction vm)))))

;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

(define (vm:step vm)
  (vm:set-instruction! vm (vector-ref (vm:code vm) (vm:pc vm)))
  (vm:set-pc! vm (+ 1 (vm:pc vm)))
  (let ((operation (vm:op (opcode (vm:instruction vm)))))
    (operation vm)))

