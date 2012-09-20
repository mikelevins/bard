;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          op.scm
;;;; Project:       Bard
;;;; Purpose:       Bard VM machine operations and opcodes 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $opfn->opcode-table (make-table test: eq?))
(define $opcode->opfn-table (make-table test: eq?))

(define (defop opcode opfn)
  (table-set! $opfn->opcode-table opfn opcode)
  (table-set! $opcode->opfn-table opcode opfn)
  opcode)

(define (opfn->opcode opfn)
  (table-ref $opfn->opcode-table opfn #f))

(define (opcode->opfn opcode)
  (table-ref $opcode->opfn-table opcode #f))

(defop 'HALT (lambda (vmstate)(vmstate-halted?-set! #t)))
(defop 'LREF (lambda (vmstate i j)(vmpush! vmstate (lref (vmstate-environment vmstate) i j))))
(defop 'LSETR (lambda (vmstate i j)(vmpush! vmstate (lsetter (vmstate-environment vmstate) i j))))
(defop 'GREF (lambda (vmstate vname)(vmpush! vmstate (gref (vmstate-globals vmstate) vname))))
(defop 'GSETR (lambda (vmstate vname)(vmpush! vmstate (gsetter (vmstate-globals vmstate) vname))))
(defop 'PUSH (lambda (vmstate k)(vmpush! vmstate k)))
(defop 'PRIM (lambda (vmstate p)(vmpush! vmstate (apply-primitive p (vmtake! vmstate (vmstate-nargs vmstate))))))
(defop 'JUMP (lambda (vmstate dest)(vmstate-pc-set! vmstate dest)))
(defop 'FJUMP (lambda (vmstate dest)(if (logical-false? (vmpop! vmstate))(vmstate-pc-set! vmstate dest))))
(defop 'TJUMP (lambda (vmstate dest)(if (logical-true? (vmpop! vmstate))(vmstate-pc-set! vmstate dest))))
(defop 'SAVE (lambda (vmstate dest)(vmpush! vmstate (make-return-record vmstate dest))))
(defop 'CALL (lambda (vmstate argcount)
               (let ((f (vmpop!))
                     (args (vmtake! vmstate argcount)))
                 (vmstate-method-set! vmstate f)
                 (vmstate-code-set! vmstate (method-code f))
                 (vmstate-environment-set! vmstate (extend-environment (method-environment f)
                                                                       (vmstate-environment vmstate)
                                                                       (make-lambda-bindings (method-parameters f)
                                                                                             args)))
                 (vmstate-pc-set! vmstate 0))))
(defop 'RETURN (lambda (vmstate)
                 (let ((val (vmpop! vstate))
                       (return-record (vmpop! vmstate)))
                   (vmpush! vmstate val)
                   (vmreturn! vmstate return-record))))
