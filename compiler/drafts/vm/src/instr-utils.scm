;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          instr-utils.scm
;;;; Project:       Bard
;;;; Purpose:       utiliities for working with instructions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

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

