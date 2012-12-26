;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instr.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine instructions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; prelude
;;; ---------------------------------------------------------------------

(define-macro (^ params . body)
  `(lambda ,params ,@body))

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

(define-macro (prog1 form . forms)
  `(let ((val ,form))
     (begin ,@forms)
     val))

;;; ---------------------------------------------------------------------
;;; instruction definitions
;;; ---------------------------------------------------------------------

(define $opname->opfn-table (make-table test: eq?))
(define $opfn->opname-table (make-table test: eq?))

(define-macro (defop opname arglist . body)
  (let ((opfn (gensym)))
    `(begin
       (define ,opname #f)
       (let ((,opfn (lambda ,arglist (begin ,@body))))
         (set! ,opname ,opfn)
         (table-set! $opname->opfn-table ',opname ,opfn)
         (table-set! $opfn->opname-table ,opfn ',opname)
         ',opname))))

;;; ---------------------------------------------------------------------
;;; auxiliary functions
;;; ---------------------------------------------------------------------

(define (%decode instruction)
  (values (car instruction)
          (cdr instruction)))

(define (%opname->opfn opname)(table-ref $opname->opfn-table opname #f))
(define (%opfn->opname opfn)(table-ref $opfn->opname-table opfn #f))

(define (%op? thing)(and (%opname->opfn thing) #t))

(define (%instr->string instr)
  (if instr
      (receive (op args)(%decode instr)
               (cond
                ((procedure? op)(%instr->string (cons (%opfn->opname op) args)))
                ((symbol? op)(object->string instr))
                (else (error (str "Unrecognized instruction: " instr)))))
      ""))


;;; ---------------------------------------------------------------------
;;; the instructions
;;; ---------------------------------------------------------------------

(defop NOP    ()  (values))
(defop CONST  (k) (%pushv! k))
(defop POP    ()  (%popv!))
(defop PRIM   (p) (%pushv! (%apply-prim p (%popnv! (%nargs)))))
(defop LREF   (l) (%pushv! (%lref l)))
(defop LSET   (l) (%lset! l (%popv!)))
(defop GREF   (g) (%pushv! (%gref g)))
(defop GSET   (g) (%gset! g (%popv!)))
(defop GO     (d) (%setpc! d))
(defop FGO    (d) (unless (%popv!)(%setpc! d)))
(defop TGO    (d) (when (%popv!)(%setpc! d)))
(defop SAVE   (d) (%pushc! (%make-saved d)))
(defop CALL   (n) (%callfn n))
(defop RETURN ()  (%return))
(defop CC     (d) (%pushv! (%makecc d)))
(defop SETCC  ()  (%setcc!))
