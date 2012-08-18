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
  instruction
  toplevel
  env
  call-stack ; used by step-exec
  module
  modules)

(define (make-vm #!key
                 (toplevel #f)
                 (env (null-env))
                 (module (%default-initial-module))
                 (modules (%bard-modules)))
  (if toplevel
      (%private-make-vm #f
                        toplevel
                        env
                        '()
                        module
                        modules)
      (error "No toplevel function supplied")))

(define current-vm (make-parameter #f))

(define (init-vm vm)
  (current-vm vm)
  (vm-instruction-set! (current-vm)
                       (function-code (vm-toplevel (current-vm)))))

(define (get-env)
  (vm-env (current-vm)))

(define (find-module mname)
  (if mname
      (get-module (vm-modules (current-vm)) mname)
      (vm-module (current-vm))))

(define (set-current-module! mname)
  (let ((module (get-module (vm-modules (current-vm)) mname)))
    (if module
        (vm-module-set! (current-vm) module)
        (error (string-append "No such module "
                              (object->string mname))))))

(define (add-module! mname)
  (let ((module (get-module (vm-modules (current-vm)) mname)))
    (if module
        (error (string-append "Module exists: "
                              (object->string mname)))
        (define-module (vm-modules (current-vm)) mname))))

(define (print-vm vm)
  (display vm))



(define (exec instr)
  (cond 
   ((null? instr) instr)
   ((list? instr)(if (procedure? (car instr))
                     (apply (car instr)
                            (map exec (cdr instr)))
                     instr))
   (else instr)))

(define (start vm)
  (init-vm vm)
  (exec (vm-instruction vm)))

;;; (define $code (%link (%compile (%read-from-string "101") (null-env))))
;;; (define $fn (make-function $code))
;;; (define $vm (make-vm toplevel: $fn))
;;; (start $vm)

;;; (define $code (%link (%compile (%read-from-string "nothing") (null-env))))
;;; (define $fn (make-function $code))
;;; (define $vm (make-vm toplevel: $fn))
;;; (start $vm)

;;; (define $env (extend-env (extend-env (null-env) 'y 1 #t) 'x 2))
;;; (define $code (%link (%compile (%read-from-string "x") $env)))
;;; (define $fn (make-function $code))
;;; (define $vm (make-vm toplevel: $fn))
;;; (start $vm)
