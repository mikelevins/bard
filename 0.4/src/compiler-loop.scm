;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-loop.scm
;;;; Project:       Bard
;;;; Purpose:       compilation of loop forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;; (loop ((a 0))
;;       (if (< a 100)
;;           (continue (+ a 1))
;;           a))

(define (bard:compile-loop expr env)
  (let* ((bindings (cadr expr))
         (body (drop 2 expr))
         (formals (map car bindings))
         (inits (map cadr bindings))
         (call-env (env:add-binding env 'continue #f))
         (continue-fn (lambda:create formals (bard:compile (cons 'begin body)) call-env)))
    (env:binding-set! (env:get-binding 'continue call-env)
                      continue-fn)
    `(,continue-fn ,@inits)))

