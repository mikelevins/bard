;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bardvm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; vm globals
;;; ----------------------------------------------------------------------

;;; globals is the root env
;;; var 0 is currently a placeholder
;;; will replace with bard globals
(define $globals 
  (let ((env (null-env))
        (gframe (make-frame '(#f))))
    (push-frame gframe env)))

;;; ----------------------------------------------------------------------
;;; vm registers
;;; ----------------------------------------------------------------------

(define $program #f)
(define $pc 0)
(define $vals (make-stack 1024)) ; size = max number of arguments/return values
(define $env $globals)
(define $haltfn #f) ; set by vm startup

;;; ----------------------------------------------------------------------
;;; running the vm
;;; ----------------------------------------------------------------------

(define (vmload code)
  (vector-map (lambda (instr)
                (let* ((out (vector-copy instr))
                       (opcode (vector-ref out 0))
                       (opfn (vector-ref $instructions opcode)))
                  (vector-set! out 0 opfn)
                  out))
              code))

(define (exec! code)
  (let* ((instr (vector-ref code $pc))
         (opfn (vector-ref instr 0)))
    (opfn instr)
    (exec! code)))

(define (vmrun code)
  (display-banner)
  (call/cc
   (lambda (exit)
     (set! $haltfn exit)
     (exec! code))))

;;; ----------------------------------------------------------------------
;;; main program
;;; ----------------------------------------------------------------------

(let ((argv (cdr (command-line))))
  (display-banner)
  (if (null? argv)
      (display-usage)
      (let* ((progpath (car argv))
             (program (load-bardo progpath)))
        (set! $program program)
        (let ((code (vmload (program-code $program))))
          (vmrun code))
        (newline)
        (display "Bard terminated.")
        (newline))))



