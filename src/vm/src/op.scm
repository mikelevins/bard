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

(define $table:opname->opfn (make-table test: eq?))
(define $table:opfn->opname (make-table test: eq?))

(define-macro (defop opname opfn)
  `(begin
     (define ,opname ,opfn)
     (table-set! $table:opname->opfn ,opname ,opfn)
     (table-set! $table:opfn->opname ,opfn ',opname)))

;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

;;; constants

(defop CONST (lambda (k) k))
(defop LREF (lambda (i j)(lref (get-env) i j)))
(defop MREF (lambda (mnm vnm)(mref (find-module mnm) vnm)))
(defop BEGIN (lambda exprs (let loop ((exprs exprs)
                                      (val #!void))
                             (if (null? exprs)
                                 val
                                 (loop (cdr exprs)
                                       (exec (car exprs)))))))
(defop SLOTSETTER (lambda (var obj)(slot-setter obj var)))
(defop LSETTER (lambda (i j)(lsetter (get-env) i j)))
(defop MSETTER (lambda (mnm vnm)(msetter (find-module mnm) vnm)))
(defop METHOD (lambda (lambda-list body-code env)(make-method lambda-list body-code env)))
(defop DEF (lambda (var val mutable?)
             (receive (vnm mnm)(parse-symbol-name s)
                      (define-variable (find-module mnm) vnm value: val mutable: mutable?))))
(defop QUOTE (lambda (x) x))
(defop APP (lambda (op args)(apply-applicable op args)))

;;; ---------------------------------------------------------------------
;;; instruction linkers
;;; ---------------------------------------------------------------------

(define $linker-table (make-table test: eq?))

(define-macro (deflink opname linkfn)
  `(table-set! $linker-table ',opname ,linkfn))

(define (%linker opname)
  (table-ref $linker-table opname #f))

(deflink CONST (lambda (expr) (cons CONST (cdr expr))))
(deflink LREF (lambda (expr) (cons LREF (cdr expr))))
(deflink MREF (lambda (expr) (cons MREF (cdr expr))))
(deflink BEGIN (lambda (expr) (cons BEGIN (map %link (cdr expr)))))

(deflink SLOTSETTER (lambda (expr) 
                      (let* ((slotname (cadr expr))
                             (objref (caddr expr)))
                        (list SLOTSETTER slotname (%link objref)))))

(deflink LSETTER (lambda (expr) 
                   (let* ((i (cadr expr))
                          (j (caddr expr)))
                     (list LSETTER i j))))

(deflink MSETTER (lambda (expr) 
                   (let* ((mname (cadr expr))
                          (vname (caddr expr)))
                     (list MSETTER mname vname))))

(deflink METHOD (lambda (expr) 
                  (let ((lambda-list (cadr expr))
                        (body-code (caddr expr))
                        (env (cadddr expr)))
                    (list METHOD lambda-list (%link body-code) env))))

(deflink DEF (lambda (expr) 
               (let ((var (cadr expr))
                     (valexpr (caddr expr))
                     (mutability (cadddr expr)))
                 (list DEF var (%link valexpr) mutability))))

(deflink QUOTE (lambda (expr) (cons QUOTE (cdr expr))))

(deflink APP (lambda (expr)
               (let ((opexpr (cadr expr))
                     (args (caddr expr)))
                 (list APP (%link opexpr) (map %link args)))))
