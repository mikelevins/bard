;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          codegen.scm
;;;; Project:       bard
;;;; Purpose:       code generation for the bard compiler
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define (comp:gen op . args)
  (case op
    ((NOOP)(list $op_NOOP))
    ((RETURN)(list $op_RETURN))
    ((CONSTANT)(list $op_CONSTANT (car args)))
    ((TRUE)(list $op_TRUE))
    ((FALSE)(list $op_FALSE))
    ((MINUSONE)(list $op_MINUSONE))
    ((ZERO)(list $op_ZERO))
    ((ONE)(list $op_ONE))
    ((TWO)(list $op_TWO))
    ((UNDEFINED)(list $op_UNDEFINED))
    ((NOTHING)(list $op_NOTHING))
    (else (error "Unrecognized operator in code generation" op))))

(define (comp:gen-special-constant val)
  (cond
   ((eqv? #t val)(comp:gen 'TRUE))
   ((eqv? #f val)(comp:gen 'FALSE))
   ((eqv? -1 val)(comp:gen 'MINUSONE))
   ((eqv? 0 val)(comp:gen 'ZERO))
   ((eqv? 1 val)(comp:gen 'ONE))
   ((eqv? 2 val)(comp:gen 'TWO))
   ((eqv? (bard:undefined) val)(comp:gen 'UNDEFINED))
   ((eqv? (bard:nothing) val)(comp:gen 'NOTHING))
   (else (error "Unrecognized special constant in code generation" val))))