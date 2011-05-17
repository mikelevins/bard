;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          ast.scm
;;;; Project:       bard
;;;; Purpose:       abstract syntax trees for the compiler
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;;---------------------------------------------------------------------
;;; abstract syntax trees
;;;---------------------------------------------------------------------

(define-type ast:node
  id: 3C4FA162-72E6-4854-ACCA-06BCC5D8BE75
  constructor: %make-ast-node
  (this ast:node-this)
  (next ast:node-next))

(define-type ast:leaf
  id: C683BF4C-3DAE-496B-A28D-DE2571ABE504
  constructor: %make-ast-leaf
  (type ast:leaf-type)
  (datum ast:leaf-datum))

(define (ast:node this #!optional (next #f))
  (%make-ast-leaf type datum))

(define (ast:leaf type datum)
  (%make-ast-leaf type datum))

(define (ast:cons it ast)
  (if (not (ast:leaf? it))
      (error "first argument to ast:cons was not an ast:leaf"))
  (if (not (or (ast:node? ast)
               (eq? ast #f)))
      (error "second argument to ast:cons was not an ast:node"))
  (%make-ast-node it ast))

(define (ast:head )
  )

(define (ast:tail )
  )