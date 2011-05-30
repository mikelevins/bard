;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       bard
;;;; Purpose:       the bardvm
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type bard:vm
  id: 5FC9B9DA-EB7F-438D-99D2-BE0EB93A8467
  constructor: bard:%make-vm
  (cells vm:%cells)
  (environment vm:%environment)
  (code vm:%code)
  (pc vm:%pc vm:%set-pc!)
  (stack vm:%stack vm:%set-stack!))

(define (bard:make-vm code env)
  (bard:%make-vm (make-table)
                 env
                 code
                 0
                 '()))

(define (bard:print-vm vm)
  (newline)
  (display "Bard VM v 0.1")
  (newline)
  (display "stack:")
  (let ((vals (vm:%stack vm)))
    (for-each (lambda (v)
                (newline)
                (display "  ")
                (display v))
              vals))
  (newline)
  (display "pc:")
  (display "  ")
  (display (vm:%pc vm))
  (newline)
  (display "code:")
  (display "  ")
  (display (vm:%code vm))
  (newline))

(define (vm:print-values vm)
  (let ((vals (vm:%stack vm)))
    (for-each (lambda (v)(newline)(bard:print-object v))
              vals)
    (newline)))

(define (vm:%opcode->op opcode)
  (table-ref $instructions opcode #f))

(define (vm:%incpc vm)
  (vm:%set-pc! vm (+ 1 (vm:%pc vm))))

(define (vm:%push vm k)
  (vm:%set-stack! vm (cons k (vm:%stack vm))))

(define (vm:%execute opcode vm args)
  (let ((op (vm:%opcode->op opcode)))
    (apply op `(,vm ,@args))))

(define (vm:%instruction-operator instr)
  (car instr))

(define (vm:%instruction-args instr)
  (cdr instr))

(define (vm:%next vm)
  (let* ((instr (list-ref (vm:%code vm)(vm:%pc vm))))
    (vm:%execute (vm:%instruction-operator instr)
                 vm
                 (vm:%instruction-args instr))))