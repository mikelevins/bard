;;;; ***********************************************************************
;;;;
;;;; Name:          macro.scm
;;;; Project:       Bard
;;;; Purpose:       bard's implementation of macros
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; TODO: AOT macroexpansion
;;;       the first step toward a compiler is an ahead-of-time
;;;       macroexpander that converts s-expressions to
;;;       fully-macroexpanded 'compiled' expressions. That's the
;;;       lowest-hanging performance fruit: it ensures that a macro
;;;       needs to be expanded only once, at compile time, not every
;;;       time the form is encountered

(define $bard-macro-functions (make-table test: eq?))

(define (%define-macro-function name mfun)
  (table-set! $bard-macro-functions name mfun))

(define (%macro-form? expr)
  (and (list? expr)
       (not (null? expr))
       (table-ref $bard-macro-functions (car expr) #f)
       #t))

(define (%macroexpand expr)
  (let* ((expander (table-ref $bard-macro-functions (car expr) #f)))
    (if expander
        (%funcall expander expr)
        (error "undefined macro in expression" expr))))

(define (%eval-macro-form expr env)
  (%eval (%macroexpand expr) env))
