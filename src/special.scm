;;;; ***********************************************************************
;;;;
;;;; Name:          special.scm
;;;; Project:       Bard
;;;; Purpose:       bard's special-form evaluator
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

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
      ((begin)(%eval-sequence (cdr expr) env))
      ((case)(%not-yet-implemented 'case))
      ((cond)(%eval-cond-body (cdr expr) env))
      ((def)(%not-yet-implemented 'def))
      ((define)(%not-yet-implemented 'define))
      ((defined?)(%not-yet-implemented 'defined?))
      ((ensure)(%not-yet-implemented 'ensure))
      ((function)(%not-yet-implemented 'function))
      ((generate)(%not-yet-implemented 'generate))
      ((if)(%eval-if expr env))
      ((let)(%not-yet-implemented 'let))
      ((loop)(%not-yet-implemented 'loop))
      ((match)(%not-yet-implemented 'match))
      ((method)(%not-yet-implemented 'method))
      ((not)(%not-yet-implemented 'not))
      ((quasiquote)(%not-yet-implemented 'quasiquote))
      ((quote)(cadr expr))
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

;;; ---------------------------------------------------------------------
;;; helper functions
;;; ---------------------------------------------------------------------

(define (%eval-sequence forms env)
  (if (null? forms)
      (bard:nothing)
      (if (null? (cdr forms))
          (bard:eval (car forms) env)
          (begin (bard:eval (car forms) env)
                 (%eval-sequence (cdr forms) env)))))

;;; ---------------------------------------------------------------------
;;; special-form evaluators
;;; ---------------------------------------------------------------------

(define (%eval-cond-body clauses env)
  (if (null? clauses)
      (bard:nothing)
      (let* ((clause (car clauses))
             (more-clauses (cdr clauses)))
        (if (bard:true? (bard:eval (car clause) env))
            (%eval-sequence (cdr clause) env)
            (%eval-cond-body more-clauses env)))))

(define (%eval-if expr env)
  (let* ((argforms (cdr expr))
         (argcount (length argforms)))
    (cond
     ((< argcount 2)(error "not enough arguments to if: " expr))
     ((= argcount 2)(let* ((testform (car argforms))
                           (thenform (cadr argforms))
                           (testval (bard:eval testform env)))
                      (if (bard:true? testval)
                          (bard:eval thenform env)
                          (bard:nothing))))
     ((= argcount 3)(let* ((testform (car argforms))
                           (thenform (cadr argforms))
                           (testval (bard:eval testform env))
                           (elseform (caddr argforms)))
                      (if (bard:true? testval)
                          (bard:eval thenform env)
                          (bard:eval elseform env))))
     (else (error "too many arguments to if: " expr)))))
