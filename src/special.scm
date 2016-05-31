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
          and
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
      ((^)(%not-yet-implemented '^))                   ; ***           
      ((->)(%not-yet-implemented '->))                 ; ***
      ((and)(%eval-and (cdr expr) env))
      ((begin)(%eval-sequence (cdr expr) env))
      ((case)(%not-yet-implemented 'case))             ; ***
      ((cond)(%eval-cond-body (cdr expr) env))
      ((def)(%eval-def expr env))
      ((define)(%not-yet-implemented 'define))         ; ***
      ((defined?)(%eval-defined? expr env))
      ((ensure)(%not-yet-implemented 'ensure))         ; ***
      ((generate)(%not-yet-implemented 'generate))     ; ***
      ((if)(%eval-if expr env))
      ((let)(%not-yet-implemented 'let))               ; ***
      ((loop)(%not-yet-implemented 'loop))             ; ***
      ((match)(%not-yet-implemented 'match))           ; ***
      ((not)(%eval-not (cadr expr) env))
      ((or)(%not-yet-implemented 'or))                 ; ***
      ((quasiquote)(%not-yet-implemented 'quasiquote)) ; ***
      ((quote)(cadr expr))
      ((receive)(%not-yet-implemented 'receive))       ; ***
      ((repeat)(%not-yet-implemented 'repeat))         ; ***
      ((send)(%not-yet-implemented 'send))             ; ***
      ((set!)(%not-yet-implemented 'set!))             ; ***
      ((setter)(%not-yet-implemented 'setter))         ; ***
      ((spawn)(%not-yet-implemented 'spawn))           ; ***
      ((undefine)(%not-yet-implemented 'undefine))     ; ***
      ((unless)(%eval-unless expr env))
      ((values)(%not-yet-implemented 'values))         ; ***
      ((when)(%eval-when expr env))
      ((with-exit)(%not-yet-implemented 'with-exit))   ; ***
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

(define (%eval-and forms env #!optional (last-val (bard:true)))
  (if (null? forms)
      last-val
      (let ((val (bard:eval (car forms) env)))
        (if (bard:true? val)
            (%eval-and (cdr forms) env val)
            (bard:false)))))

(define (%eval-cond-body clauses env)
  (if (null? clauses)
      (bard:nothing)
      (let* ((clause (car clauses))
             (more-clauses (cdr clauses)))
        (if (bard:true? (bard:eval (car clause) env))
            (%eval-sequence (cdr clause) env)
            (%eval-cond-body more-clauses env)))))


(define (%eval-def expr env)
  (let* ((argforms (cdr expr))
         (argcount (length argforms)))
    (cond
     ((< argcount 2)(error "too few arguments to def: " argforms))
     ((= argcount 2)(let* ((varform (car argforms))
                           (valform (cadr argforms)))
                      (if (symbol? varform)
                          (%defglobal varform (bard:eval valform env))
                          (error "the first argument to def must by a symbol, but found " varform))))
     (else (error "too many arguments to def: " argforms)))))

(define (%eval-defined? expr env)
  (let* ((argforms (cdr expr))
         (argcount (length argforms)))
    (cond
     ((< argcount 1)(error "not enough arguments to defined? " expr))
     ((= argcount 1)(let* ((var (car argforms))
                           (val (%lookup-variable-value env var)))
                      (if (%defined? val)
                          (bard:true)
                          (let ((global-val (%global-value var)))
                            (if (%defined? global-val)
                                (bard:true)
                                (bard:false))))))
     (else (error "too many arguments to defined? " expr)))))

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

(define (%eval-not form env)
  (if (bard:true? (bard:eval form env))
      (bard:false)
      (bard:true)))

(define (%eval-unless expr env)
  (let* ((argforms (cdr expr))
         (argcount (length argforms)))
    (cond
     ((< argcount 1)(error "not enough arguments to unless: " expr))
     (else (let* ((testform (car argforms))
                  (more-forms (cdr argforms))
                  (testval (bard:eval testform env)))
             (if (bard:false? testval)
                 (%eval-sequence more-forms env)
                 (bard:nothing)))))))

(define (%eval-when expr env)
  (let* ((argforms (cdr expr))
         (argcount (length argforms)))
    (cond
     ((< argcount 1)(error "not enough arguments to when: " expr))
     (else (let* ((testform (car argforms))
                  (more-forms (cdr argforms))
                  (testval (bard:eval testform env)))
             (if (bard:true? testval)
                 (%eval-sequence more-forms env)
                 (bard:nothing)))))))
