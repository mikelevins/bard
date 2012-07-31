;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          op.scm
;;;; Project:       Bard VM
;;;; Purpose:       vm operations
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $table:opcodes->opfns (make-table test: eqv?))
(define $table:opfns->opcodes (make-table test: eqv?))
(define $table:opnames->opcodes (make-table test: eqv?))
(define $table:opcodes->opnames (make-table test: eqv?))

(define (define-opcode opc opfn)
  (table-set! $table:opcodes->opfns opc opfn)
  (table-set! $table:opfns->opcodes opfn opc)
  opc)

(define (define-opname opnm opc)
  (table-set! $table:opnames->opcodes opnm opc)
  (table-set! $table:opcodes->opnames opc opnm)
  opnm)

(define-macro (defop opc opnm vmnm iargs . body)
  `(begin
     (define ,opnm ,opc)
     (define-opcode ,opc (lambda (,@vmnm ,@iargs) (begin ,@body)))
     (define-opname ',opnm ,opc)
     ',opnm))

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

(defop  1 VAL     (vm) (k)   (pushval! vm k))
(defop  2 LREF    (vm) (i j) (pushval! vm (lref (env vm) i j)))
(defop  3 LSETR   (vm) (i j) (pushval! vm (lsetter (env vm) i j)))
(defop  4 MREF    (vm) (m v) (pushval! vm (mref (find-module (modules vm) m) v)))
(defop  5 MSETR   (vm) (m v) (pushval! vm (msetter (find-module vm m) v)))
(defop  6 SREF    (vm) ()    (pushval! vm (let ((obj (popval! vm))
                                                (slot-name (popval! vm)))
                                            (slotref obj slot-name))))

(defop  7 SSETR   (vm) ()    (pushval! vm (let ((obj (popval! vm))
                                                (slot-name (popval! vm)))
                                            (slotsetter obj slot-name))))

(defop  8 DEF     (vm) ()    (pushval! vm (let ((mutable? (popval! vm))
                                                (val (popval! vm))
                                                (varname (popval! vm))
                                                (module (popval! vm)))
                                            (define-variable module varname val mutable?))))

(defop  9 CLOSE   (vm) ()    (pushval! vm (let ((env (popval! vm))
                                                (code-body (popval! vm))
                                                (lambda-list (popval! vm)))
                                            (make-closure lambda-list code-body env))))

(defop 10 PRIM    (vm) ()    (let* ((prim (popval! vm))
                                    (nargs (nvals vm))
                                    (args (popnvals! vm nargs)))
                               (call-with-values
                                   (lambda () (apply-primitive prim args))
                                 (lambda vals (pushvals! vm vals)))))

(defop 11 MODULE  (vm) (nm)  (pushval! (find-module vm nm)))

(defop 12 INMOD   (vm) (nm)  (pushval! (set-module! vm (find-module vm nm))))

(defop 13 JUMP    (vm) (dst) (pc-set! vm dst))
(defop 14 TJUMP   (vm) (dst) (if (popval! vm)(pc-set! vm dst)))
(defop 15 FJUMP   (vm) (dst) (if (not (popval! vm))(pc-set! vm dst)))

(defop 16 SAVE    (vm) (dst) (pushstate! vm dst (code vm)(fn vm)(stack vm)(env vm)(module vm)))

(defop 17 APPLY   (vm) ()    (let* ((fn (popval! vm))
                                    (lambda-list (function-lambda-list fn))
                                    (args (popnvals! vm (compute-nvals vm lambda-list)))
                                    (env (extend-function-env function-env (env vm) fn lambda-list args)))
                               (setstate! vm 0 (function-code fn) fn (stack vm) env (module vm))))

(defop 18 RETURN  (vm) (dst) (let ((state (popstate! vm)))
                               (setstate! vm (pc state) (code state) (fn state) (stack state)
                                          (env state) (module state))))

