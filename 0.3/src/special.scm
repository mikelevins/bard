;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard 0.3
;;;; Purpose:       compilation of special-forms
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%compile-and expr env modules) #f)
(define (%compile-begin expr env modules) #f)
(define (%compile-define expr env modules) #f)
(define (%compile-define-function expr env modules) #f)
(define (%compile-define-macro expr env modules) #f)
(define (%compile-schema expr env modules) #f)
(define (%compile-function expr env modules) #f)
(define (%compile-generate expr env modules) #f)
(define (%compile-if expr env modules) #f)
(define (%compile-let expr env modules) #f)
(define (%compile-match expr env modules) #f)
(define (%compile-method expr env modules) #f)
(define (%compile-not expr env modules) #f)
(define (%compile-quasiquote expr env modules) #f)
(define (%compile-quote expr env modules) #f)
(define (%compile-repeat expr env modules) #f)
(define (%compile-setter expr env modules) #f)
(define (%compile-time expr env modules) #f)
(define (%compile-unquote expr env modules) #f)
(define (%compile-unquote-splicing expr env modules) #f)
(define (%compile-with-exit expr env modules) #f)

(define (%compile-special-form expr env modules)
  (let ((op (%car expr)))
    (case op
      ((and)(%compile-and expr env modules))
      ((begin)(%compile-begin expr env modules))
      ((define)(%compile-define expr env modules))
      ((define-function)(%compile-define-function expr env modules))
      ((define-macro)(%compile-define-macro expr env modules))
      ((define-schema)(%compile-schema expr env modules))
      ((function)(%compile-function expr env modules))
      ((generate)(%compile-generate expr env modules))
      ((if)(%compile-if expr env modules))
      ((let)(%compile-let expr env modules))
      ((match)(%compile-match expr env modules))
      ((method)(%compile-method expr env modules))
      ((not)(%compile-not expr env modules))
      ((quasiquote)(%compile-quasiquote expr env modules))
      ((quote)(%compile-quote expr env modules))
      ((repeat)(%compile-repeat expr env modules))
      ((setter)(%compile-setter expr env modules))
      ((time)(%compile-time expr env modules))
      ((unquote)(%compile-unquote expr env modules))
      ((unquote-splicing)(%compile-unquote-splicing expr env modules))
      ((with-exit)(%compile-with-exit expr env modules))
      (else (error (string-append "Compiler error: unrecognized special form: "
                                  (object->string op)))))))

