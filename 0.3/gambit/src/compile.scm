;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; stubs
;;; ---------------------------------------------------------------------

(define (%find-variable env expr) (values #f #f))

;;; ---------------------------------------------------------------------
;;; discriminating expression types
;;; ---------------------------------------------------------------------

(define (%self-evaluating? expr)
  (and (not (symbol? expr))
       (not (pair? expr))))

(define (%special-form? expr)
  (memq expr '(λ ^ lambda method begin cond define ensure if let loop
                 match quasiquote quote setter set! unless unquote
                 unquote-splicing when with-exit)))

(define (%macro-form? expr)
  #f)

(define (%lambda-expression? expr)
  (and (pair? expr)
       (memq (car expr) '(λ ^ lambda method))))

;;; ---------------------------------------------------------------------
;;; code generators
;;; ---------------------------------------------------------------------

(define (%seq . code)
  (apply append code))

(define (%gen opcode . args)
  (list (cons opcode args)))

(define %next-label-number #f)
(let ((_labelnum 0))
  (set! %next-label-number
        (lambda ()
          (set! _labelnum (+ 1 _labelnum))
          _labelnum)))

(define (%gen-label #!optional (name 'L))
  (string->symbol (str name (%next-label-number))))

(define (%gen-none) '())

(define (%gen-var expr env) 
  (receive (i j)(%find-variable env expr)
           (if i
               (%gen 'LREF i j "; " expr)
               (%gen 'GREF expr))))

;;; ---------------------------------------------------------------------
;;; expression compilers
;;; ---------------------------------------------------------------------

(define (%compile-not-yet-implemented name expr env val? more?)
  (error (str "Compiler not yet implemented for " name)))

(define (%compile-application fexpr argexprs env val? more?)
  (let ((prim (%primitive? fexpr)))
    (cond
     (prim (if (and (not val?)(not (%prim-side-effects? prim)))
               ;; side-effect free primitive call, value not needed
               (%compile-begin argexprs env #f more?)
               ;; value or side-effect needed
               (%seq (%compile-list argexprs env val? more?)
                     (%gen (%prim-opname prim)(length argexprs))
                     (if val? '() (%gen 'POP))
                     (if more? '() (%gen 'RETURN)))))
     ((and (%lambda-expression? fexpr)
           (null? (cadr fexpr)))
      ;; (lambda () . body) -> (begin . body)
      (assert (null? argexprs) "Too many arguments")
      (%compile-begin (drop 2 fexpr) env val? more?))
     (more? ;; must save the return continuation
      (let ((k (%gen-label 'k)))
        (%seq (%gen 'SAVE k)
              (%compile-list argexprs env val? more?)
              (%compile fexpr env #t #t)
              (%gen 'CALLJ (length argexprs))
              (list k)
              (if val? '() (%gen 'POP)))))
     (else ;; the function call is a simple jump 
      (%seq (%compile-list argexprs env val? more?)
            (%compile fexpr env #t #t)
            (%gen 'CALLJ (length argexprs)))))))

(define (%compile-begin exprs env val? more?)
  (cond
   ((null? exprs)(%compile-constant '() env val? more?))
   ((= 1 (length exprs))(%compile (car exprs) env val? more?))
   (else (%seq (%compile (car exprs) env #f #t)
               (%compile-begin (cdr exprs) env val? more?)))))

(define (%compile-cond expr env val? more?)
  (%compile-not-yet-implemented 'cond expr env val? more?))

(define (%compile-constant expr env val? more?)
  (if val?
      (%seq (%gen 'CONST expr)
            (if more? '() (%gen 'RETURN)))
      (%gen-none)))

(define (%compile-define-class expr env val? more?)
  (%compile-not-yet-implemented '(define class) expr env val? more?))

(define (%compile-define-macro expr env val? more?)
  (%compile-not-yet-implemented '(define macro) expr env val? more?))

(define (%compile-define-method expr env val? more?)
  (%compile-not-yet-implemented '(define method) expr env val? more?))

(define (%compile-define-protocol expr env val? more?)
  (%compile-not-yet-implemented '(define protocol) expr env val? more?))

(define (%compile-define-record expr env val? more?)
  (%compile-not-yet-implemented '(define record) expr env val? more?))

(define (%compile-define-variable expr env val? more?)
  (%compile-not-yet-implemented '(define variable) expr env val? more?))

(define (%compile-define-vector expr env val? more?)
  (%compile-not-yet-implemented '(define vector) expr env val? more?))

(define (%compile-define expr env val? more?)
  (case (cadr expr)
    ((class)(%compile-define-class (cddr expr) env val? more?))
    ((macro)(%compile-define-macro (cddr expr) env val? more?))
    ((method)(%compile-define-method (cddr expr) env val? more?))
    ((protocol)(%compile-define-protocol (cddr expr) env val? more?))
    ((record)(%compile-define-record (cddr expr) env val? more?))
    ((variable)(%compile-define-variable (cddr expr) env val? more?))
    ((vector)(%compile-define-vector (cddr expr) env val? more?))
    (else (error (str "Unrecognized definition: define" (cadr expr))))))

(define (%compile-ensure expr env val? more?)
  (%compile-not-yet-implemented 'ensure expr env val? more?))

(define (%compile-if test then else env val? more?)
  (let* ((testcode (%compile test env #t #t))
         (thencode (%compile then env val? more?))
         (elsecode (%compile else env val? more?)))
    (cond
     ((equal? thencode elsecode)(%seq (%compile test env nil t)
                                      elsecode))
     ((null? thencode)(let ((L2 (%gen-label)))
                        (%seq testcode
                              (%gen 'TJUMP L2)
                              elsecode
                              (list L2)
                              (if more? '() (%gen 'RETURN)))))
     ((null? elsecode)(let ((L1 (%gen-label)))
                        (%seq testcode
                              (%gen 'FJUMP L1)
                              thencode
                              (if more? '() (%gen 'RETURN)))))
     (else (let ((L1 (%gen-label))
                 (L2 (if more? (%gen-label) '())))
             (%seq testcode
                   (%gen 'FJUMP L1)
                   thencode
                   (if more? (%gen 'JUMP L2) '())
                   (list L1)
                   elsecode
                   (if more? (list L2) '())))))))

(define (%compile-let expr env val? more?)
  (%compile-not-yet-implemented 'let expr env val? more?))

(define (%compile-list expr env val? more?)
  (if (null? expr)
      '()
      (%seq (%compile (car expr) env #t #t)
            (%compile-list (cdr expr) env val? more?))))

(define (%compile-loop expr env val? more?)
  (%compile-not-yet-implemented 'loop expr env val? more?))

(define (%compile-match expr env val? more?)
  (%compile-not-yet-implemented 'match expr env val? more?))

(define (%compile-method params body env val? more?)
  (%make-fn env: env parameters: params 
            code: (%seq (%gen-args params 0)
                        (%compile-begin body (%add-frame env (%params->frame params)) #t #f))))

(define (%compile-quasiquote expr env val? more?)
  (%compile-not-yet-implemented 'quasiquote expr env val? more?))

(define (%compile-quote expr env val? more?)
  (%compile-constant expr env val? more?))

(define (%compile-setter expr env val? more?)
  (%compile-not-yet-implemented 'setter expr env val? more?))

(define (%compile-set! expr env val? more?)
  (%compile-not-yet-implemented 'set! expr env val? more?))

(define (%compile-unquote expr env val? more?)
  (%compile-not-yet-implemented 'unquote expr env val? more?))

(define (%compile-unquote-splicing expr env val? more?)
  (%compile-not-yet-implemented 'unquote-splicing expr env val? more?))

(define (%compile-variable expr env val? more?)
  (if val? 
      (%seq (%gen-var expr env)
            (if more? '() (%gen 'RETURN)))
      '()))

(define (%compile-when expr env val? more?)
  (%compile-not-yet-implemented 'when expr env val? more?))

(define (%compile-with-exit expr env val? more?)
  (%compile-not-yet-implemented 'with-exit expr env val? more?))

(define (%compile-special-form expr env val? more?)
  (case (car expr)
    ((λ ^ lambda method)(%compile-method (cadr expr) (cddr expr) env val? more?))
    ((begin)(%compile-begin (cdr expr) env val? more?))
    ((cond)(%compile-cond expr env val? more?))
    ((define)(%compile-define expr env val? more?))
    ((ensure)(%compile-ensure expr env val? more?))
    ((if)(%compile-if (list-ref expr 1)
                      (list-ref expr 2)
                      (if (null? (drop 3 expr))
                          '()
                          (list-ref expr 3))
                      env val? more?))
    ((let)(%compile-let expr env val? more?))
    ((loop)(%compile-loop expr env val? more?))
    ((match)(%compile-match expr env val? more?))
    ((quasiquote)(%compile-quasiquote expr env val? more?))
    ((quote)(%compile-quote (cadr expr) env val? more?))
    ((setter)(%compile-setter expr env val? more?))
    ((set!)(%compile-set! expr env val? more?))
    ((unless)(%compile-if (list 'not
                                (list-ref expr 1))
                          (cons 'begin (drop 2 expr))
                          env val? more?))
    ((unquote)(%compile-unquote expr env val? more?))
    ((unquote-splicing)(%compile-unquote-splicing expr env val? more?))
    ((when)(%compile-if (list-ref expr 1)
                        (cons 'begin (drop 2 expr))
                        env val? more?))
    ((with-exit)(%compile-with-exit expr env val? more?))
    (else (error (str "Unrecognized special form: " (car expr))))))

;;; ---------------------------------------------------------------------
;;; main compiler entry point
;;; ---------------------------------------------------------------------

(define (%compile expr env val? more?)
  (cond
   ((%self-evaluating? expr)(%compile-constant expr env val? more?))
   ((symbol? expr)(%compile-variable expr env val? more?))
   ((%special-form? (car expr))(%compile-special-form  expr env val? more?))
   ((%macro-form? expr)(%compile  (%macroexpand expr) env val? more?))
   (else (%compile-application (car expr) (cdr expr) env val? more?))))


