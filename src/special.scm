;;;; ***********************************************************************
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard's special-form evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

;;; TODO: special-form efficiency
;;;       this evaluator for special forms is conceptually
;;;       convenient, but could be made more efficient;
;;;       for instance, there's really no need for 
;;;       special forms to be looked up at run time

(declare (standard-bindings))

(define (%special-form? expr)
  (memq (car expr) 
        '(^
          ->
          begin
          case
          cond
          def
          define
          defined?
          ensure
          function
          generate
          if
          let
          loop
          match
          method
          not
          quasiquote
          quote
          receive
          repeat
          send
          set!
          setter
          spawn
          undefine
          unless
          values
          when
          with-exit)))

(define (%eval-special-form expr env)
  (let* ((op (car expr))
         (args (cdr expr)))
    (case op
      ((^)(%not-yet-implemented '^))
      ((->)(%not-yet-implemented '->))
      ((begin)(%not-yet-implemented 'begin))
      ((case)(%not-yet-implemented 'case))
      ((cond)(%not-yet-implemented 'cond))
      ((def)(%not-yet-implemented 'def))
      ((define)(%not-yet-implemented 'define))
      ((defined?)(%not-yet-implemented 'defined?))
      ((ensure)(%not-yet-implemented 'ensure))
      ((function)(%not-yet-implemented 'function))
      ((generate)(%not-yet-implemented 'generate))
      ((if)(%not-yet-implemented 'if))
      ((let)(%not-yet-implemented 'let))
      ((loop)(%not-yet-implemented 'loop))
      ((match)(%not-yet-implemented 'match))
      ((method)(%not-yet-implemented 'method))
      ((not)(%not-yet-implemented 'not))
      ((quasiquote)(%not-yet-implemented 'quasiquote))
      ((quote)(%not-yet-implemented 'quote))
      ((receive)(%not-yet-implemented 'receive))
      ((repeat)(%not-yet-implemented 'repeat))
      ((send)(%not-yet-implemented 'send))
      ((set!)(%not-yet-implemented 'set!))
      ((setter)(%not-yet-implemented 'setter))
      ((spawn)(%not-yet-implemented 'spawn))
      ((undefine)(%not-yet-implemented 'undefine))
      ((unless)(%not-yet-implemented 'unless))
      ((values)(%not-yet-implemented 'values))
      ((when)(%not-yet-implemented 'when))
      ((with-exit)(%not-yet-implemented 'with-exit))
      (else (error "unrecognized special form: " expr)))))
