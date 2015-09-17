;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-function.scm
;;;; Project:       Bard
;;;; Purpose:       compilation of -> forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; (-> input-type1 input-type2 ... -> output-type1 output-type2 ...)
(define (bard:compile-function expr env)
  (let* ((args (cdr expr))
         (arrow-pos (position-if (lambda (a)(eq? a '->)) args)))
    (if arrow-pos
        (let* ((input-types (take arrow-pos args))
               (compiled-input-types (map (lambda (it)(bard:compile it env))
                                          input-types))
               (output-types (drop (+ 1 arrow-pos) args))
               (compiled-output-types (map (lambda (ot)(bard:compile ot env))
                                           output-types)))
          `(FUNCTION ,input-types ,output-types))
        (error "Syntax error in function form: " expr))))
