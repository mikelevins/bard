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
;;; syntax->value
;;; ---------------------------------------------------------------------

(define (comp:syntax-value->bard-value expr)
  (let* ((syntax-type (frame:get expr type:))
         (syntax-value (frame:get expr value:)))
    (case syntax-type
      ((undefined)(bard:undefined))
      ((nothing)(bard:nothing))
      ((boolean)(if syntax-value
                    (bard:true)
                    (bard:false)))
      ((integer) syntax-value)
      ((flonum) syntax-value)
      ((ratnum) syntax-value)
      ((character) syntax-value)
      ((name)(let ((mname (frame:get expr module-name:))
                   (vname syntax-value))
               (if mname
                   (string->symbol (string-append (object->string mname) ":" (object->string vname)))
                   vname)))
      ((text) syntax-value)
      ((sequence)(seq:make-sequence (map comp:syntax-value->bard-value syntax-value)))
      ((application)(seq:make-sequence (map comp:syntax-value->bard-value syntax-value)))
      ((frame)(frame:plist->frame (map comp:syntax-value->bard-value syntax-value)))
      (else (error "Unknown type of syntax object" obj)))))

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

(define (comp:application-type expr)
  '())

;;; ---------------------------------------------------------------------
;;; typed expression compilers
;;; ---------------------------------------------------------------------

(define (comp:special-constant? val)
  (or (eqv? #t val)
      (eqv? #f val)
      (eqv? -1 val)
      (eqv? 0 val)
      (eqv? 1 val)
      (eqv? 2 val)
      (eqv? (bard:undefined) val)
      (eqv? (bard:nothing) val)))

(define (bard:compile-constant expr val? more?)
  (if val?
      (let* ((val (comp:syntax-value->bard-value expr))
             (epilogue (if more? '() (comp:gen 'RETURN)))
             (body (if (comp:special-constant? val)
                       (comp:gen-special-constant val)
                       (comp:gen 'CONSTANT val))))
        (append body epilogue))
      '()))

(define (bard:compile-name-expression expr env val? more?)
  (if val?
      (let* ((nm (comp:syntax-value->bard-value expr))
             (epilogue (if more? '() (comp:gen 'RETURN)))
             (body (comp:gen-variable-reference nm env)))
        (append body epilogue))
      '()))

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

