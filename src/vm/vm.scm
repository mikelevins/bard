;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-vm-version "0.3.0")

(define-type vm
  id: 37C7FA3E-3C9A-45E0-B70D-CE1D252BF17B
  constructor: %private-make-vm
  pc
  instruction
  code
  fn
  vals
  stack
  env
  module
  modules)


(define (make-vm #!key (fn #f)(args '())(env (null-env))(module #f))
  (let ((code (if fn (function-code fn) #f))
        (module (or module (%default-initial-module)))
        (modules (%bard-modules)))
    (%private-make-vm 0
                      #f
                      code
                      fn
                      args
                      '()
                      env
                      module
                      modules)))

(define (vm-push-val! vm val)
  (vm-vals-set! vm (cons val (vm-vals vm))))

(define (vm-pop-val! vm)
  (let ((val (car (vm-vals vm))))
    (vm-vals-set! vm (cdr (vm-vals vm)))
    val))

(define (vm-get-module vm mname)
  (if (null? mname)
      (error "No valid module found")
      (if mname
          (or (find-module (vm-modules vm) mname)
              (error (string-append "Undefined module: " (object->string mname))))
          (vm-module vm))))

;;; ---------------------------------------------------------------------
;;; running
;;; ---------------------------------------------------------------------

(define (vmfetch! vm)
  (vm-instruction-set! vm (vector-ref (vm-code vm)(vm-pc vm))))

(define (vminc! vm)
  (vm-pc-set! vm (+ 1 (vm-pc vm))))

(define (vmexec! vm op args)
  (apply (op vm) args))

(define (step vm)
  (vmfetch! vm)
  (vminc! vm)
  (vmexec! vm (%op (vm-instruction vm))(%args (vm-instruction vm))))

;;; ---------------------------------------------------------------------
;;; printing vm state
;;; ---------------------------------------------------------------------


(define (print-instruction instr)
  (if (and instr (not (null? instr)))
      (let* ((op (%op instr))
             (nm (op->opname op))
             (args (%args instr)))
        (display nm)
        (if (eq? nm 'PRIM)
            (let ((pname (prim->primname (car args))))
              (display " ")(display pname))
            (for-each (lambda (arg)(display " ")(display arg))
                      args)))))

(define (print-code vm code)
  (if code
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
              (newline))))))

(define (print-modules module-registry)
  (if module-registry
      (table-for-each (lambda (k v)(newline)(display "    ")(display k))
                      module-registry)))

(define (vmprint vm)
  (newline)
  (display "     pc: ")(display (vm-pc vm))(newline)
  (display "   inst: ")(print-instruction (vm-instruction vm))(newline)
  (display "   code: ")(print-code vm (vm-code vm))(newline)
  (display "   vals: ")(display (vm-vals vm))(newline)
  (display "  stack: ")(display (vm-stack vm))(newline)
  (display "    env: ")(display (vm-env vm))(newline)
  (begin
    (display " module ")(display "[")(display (module-name (vm-module vm)))(display "]")
    (newline))
  (display "modules:")(print-modules (vm-modules vm))(newline)
  (newline))

;;; ---------------------------------------------------------------------
;;; testing
;;; ---------------------------------------------------------------------

(define (testvm expr)
  (let* ((fn (%compile-function expr (null-env)))
         (code (function-code fn))
     (vm (make-vm fn: fn)))
    (vm-code-set! vm code)
    vm))

(define (teststep vm)
  (step vm)
  (vmprint vm))

;;; (define $vm (testvm 5))
;;; (vmprint $vm)
;;; (teststep $vm)
;;;

