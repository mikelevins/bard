;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       bard
;;;; Purpose:       the bard compiler
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; application-expression compilers
;;; ---------------------------------------------------------------------

(define (bard:compile-special-form-application expr env val? more?)
  '())

(define (bard:compile-macro-application expr env val? more?)
  '())

;;; this one handles methods, generic functions, frames, and sequences
(define (bard:compile-value-application expr env val? more?)
  '())

;;; ---------------------------------------------------------------------
;;; typed expression compilers
;;; ---------------------------------------------------------------------

(define (bard:compile-constant expr val? more?)
  '())

(define (bard:compile-name-expression expr env val? more?)
  '())

(define (bard:compile-sequence-expression expr env val? more?)
  '())

(define (bard:compile-application expr env val? more?)
  (let ((application-type (comp:application-type expr)))
    (case application-type
      ((special-form)(bard:compile-special-form-application expr env val? more?))
      ((macro)(bard:compile-macro-application expr env val? more?))
      ((value)(bard:compile-value-application expr env val? more?))
      (else (error "Unknown type of application" obj)))))

(define (bard:compile-frame-expression expr env val? more?)
  '())

;;; ---------------------------------------------------------------------
;;; top-level compiler
;;; ---------------------------------------------------------------------

(define (bard:compile expr env val? more?)
  (let ((syntax-type (frame:get expr type:)))
    (case syntax-type
      ((undefined)(bard:compile-constant expr val? more?))
      ((nothing)(bard:compile-constant expr val? more?))
      ((boolean)(bard:compile-constant expr val? more?))
      ((integer)(bard:compile-constant expr val? more?))
      ((flonum)(bard:compile-constant expr val? more?))
      ((ratnum)(bard:compile-constant expr val? more?))
      ((character)(bard:compile-constant expr val? more?))
      ((name)(bard:compile-name-expression expr env val? more?))
      ((text)(bard:compile-constant expr val? more?))
      ((sequence)(bard:compile-sequence-expression expr env val? more?))
      ((application)(bard:compile-application expr env val? more?))
      ((frame)(bard:compile-frame-expression expr env val? more?))
      (else (error "Unknown type of syntax object" obj)))))