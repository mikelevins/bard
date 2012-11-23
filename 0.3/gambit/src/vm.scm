;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define-type %vmstate
  constructor: %private-make-vmstate
  (%instr %instr %setinstr!)
  (%code %code %setcode!)
  (%pc %pc %setpc!)
  (%fn %fn %setfn!)
  (%env %env %setenv!)
  (%globals %globals %setglobals!)
  (%vstack %vstack %setvstack!)
  (%cstack %cstack %setcstack!)
  (%exitfn %exitfn %setexitfn!))

(define (%makevmstate fn env globals)
  (let* ((code (%fn-code fn))
         (pc 0)
         (instr (vector-ref code pc))
         (vstack '())
         (cstack '())
         (exitfn #f))
    (%private-make-vmstate instr code pc fn env globals vstack cstack exitfn)))

(define (%make-saved state)
  (let ((instr (%instr state))
        (code (%code state))
        (pc (%pc state))
        (fn (%fn state))
        (env (%env state))
        (globals (%globals state))
        (vstack (%vstack state))
        (cstack (%cstack state))
        (exitfn (%exitfn state)))
    (%private-make-vmstate instr code pc fn env globals vstack cstack exitfn)))

(define (%copystate! srcstate deststate)
  (%setinstr! deststate (%instr srcstate))
  (%setcode! deststate (%code srcstate))
  (%setpc! deststate (%pc srcstate))
  (%setfn! deststate (%fn srcstate))
  (%setenv! deststate (%env srcstate))
  (%setglobals! deststate (%globals srcstate))
  (%setvstack! deststate (%vstack srcstate))
  (%setcstack! deststate (%cstack srcstate))
  (%setexitfn! deststate (%exitfn srcstate)))

(define (%op state)(car (%instr state)))
(define (%arg1 state)(list-ref (%instr state) 1))
(define (%arg2 state)(list-ref (%instr state) 2))
(define (%code-ref state i)(vector-ref (%code state) i))
(define (%pushv! state val)(%setvstack! state (cons val (%vstack state))))
(define (%topv! state)(car (%vstack state)))
(define (%popv! state)(let ((val (car (%vstack state)))) (%setvstack! state (cdr (%vstack state))) val))
(define (%takeallv! state)(let ((vals (%vstack state))) (%setvstack! state '()) vals))
(define (%pushc! state val)(%setcstack! state (cons val (%cstack state))))
(define (%topc! state)(car (%cstack state)))
(define (%popc! state)(let ((saved (car (%cstack state)))) (%setcstack! state (cdr (%cstack state))) saved))
(define (%cc state)(%pushv! state (%make-saved state)))
(define (%setcc! state)(%copystate! (%popv! state) state))

(define (%apply-prim state)
  (let* ((pname (%arg1 state))
         (prim (%getprim pname))
         (pfn (%prim-opfn prim))
         (args (%takeallv! state)))
    (apply pfn args)))

;;; ---------------------------------------------------------------------
;;; vm ops
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname opfn)
  `(begin
     (table-set! $opname->opfn-table ',opname ,opfn)
     (table-set! $opfn->opname-table ,opfn ',opname)
     ',opname))

(define (%opname->op opname)
  (table-ref $opname->opfn-table opname))

(defop NOP    identity)
(defop HALT   (lambda (state) ((%exitfn state)(%topv state))))
(defop CONST  (lambda (state) (%pushv! state (%arg1 state))))
(defop LREF   (lambda (state) (%pushv! state (%lref (%env state) (%arg1 state)(%arg2 state)))))
(defop LSET   (lambda (state) (%pushv! state (%lset! (%env state) (%arg1 state)(%arg2 state)(%topv state)))))
(defop GREF   (lambda (state) (%pushv! state (%gref (%env state) (%arg1 state)))))
(defop GSET   (lambda (state) (%pushv! state (%gset! (%env state) (%arg1 state)(%topv state)))))
(defop POPV   %popv!)
(defop POPC   %popc!)
(defop PRIM   (lambda (state) (%pushv! state (%apply-prim state))))
(defop JUMP   (lambda (state) (%setpc! state (%arg1 state))))
(defop FJUMP  (lambda (state) (unless (%topv state)(%set-pc! state (%arg1 state)))))
(defop TJUMP  (lambda (state) (when (%topv state)(%set-pc! state (%arg1 state)))))
(defop CALL   (lambda (state) (error "CALL not yet implemented")))
(defop RETURN (lambda (state) (error "RETURN not yet implemented")))

;;; ---------------------------------------------------------------------
;;; vm execution
;;; ---------------------------------------------------------------------

(define (%fetch! state)
  (%setinstr! state (%code-ref state (%pc state))))

(define (%inc! state)
  (%setpc! state (+ 1 (%pc state))))

(define (%exec! state)
  ((%op state) state))

(define (%stepvm state)
  (%fetch! state)
  (%inc! state)
  (%exec! state)
  state)

(define (%printstate state)
  (newline)
  (display "Bard VM v 0.3.0")(newline)
  (display (str " instr: " (%instr state)))(newline)
  (display (str " code: " (%code state)))(newline)
  (display (str " pc: " (%pc state)))(newline)
  (display (str " fn: " (%fn state)))(newline)
  (display (str " env: " (%env state)))(newline)
  (display (str " globals: " (%globals state)))(newline)
  (display (str " vstack: " (%vstack state)))(newline)
  (display (str " cstack: " (%cstack state)))(newline)
  (display (str " exitfn: " (%exitfn state)))(newline))

;;; (define $code (%link (->code `(CONST 3) `(CONST 2) `(POPV) `(HALT))))
;;; (define $fn (%makefn '() $code name: 'testfn))
;;; (define $vmstate (%makevmstate $fn (%null-env) '()))
;;; (%printstate $vmstate)
;;; (%stepvm $vmstate)
