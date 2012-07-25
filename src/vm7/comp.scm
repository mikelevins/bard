;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          comp.scm
;;;; Project:       Bard
;;;; Purpose:       Bard compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define *label-number* 0)

(define (%symbol? x)
  (or (symbol? x)(keyword? x)))

(define (%keyword? x)
  (keyword? x))

(define %nothing? null?)
(define %list? list?)

(define (%in-env? v env)
  (assq v env))

(define (%setter? x)
  (and (list? x)
       (eq? 'setter (car x))))

(define (%macro? x) #f)
(define (%macroexpand x) #f)

(define-macro (%gen opname . args)
  `(list (list ',opname ,@args)))

(define (%seq . code)(apply append code))

(define (gen-label #!optional (label "L"))
  (set! *label-number* (+ 1 *label-number*))
  (string->symbol (string-append label (object->string *label-number*))))

(define (%gen-var-ref expr env)
  (if (%in-env? expr env)
      (%gen LVAR expr)
      (%gen MVAR expr)))

(define (%compile-setter expr env)
  'setter-not-yet-implemented)

(define (%compile-begin expr env)
  (cond
   ((null? expr)(%gen NOTHING))
   ((= 1 (length expr))(compile-bard (car expr) env))
   (else (%seq (compile-bard (car expr) env)
               (%gen POP)
               (%compile-begin (cdr expr) env)))))

(define (%compile-application expr env)
  (if (%setter? (car expr))
      (%compile-setter expr env)
      (if (%macro? (car expr))
          (compile-bard (%macroexpand expr) env)
          (case (car expr)
            ((quote)(%gen CONST (cadr expr)))
            ((begin)(%compile-begin (cdr expr) env))
            ((if)(%compile-if (cdr expr) env))
            ((method)(receive (arglist method-body)(%compile-method expr env)
                              (%seq (%gen CONST arglist)(%gen CONST method-body)(%gen METH))))
            (else (let ((f (compile-bard (car expr) env))
                        (args (map (lambda (x)(compile-bard x env))
                                   (cdr expr))))
                    (%seq (%gen CONST f)(%gen CONST args)(%gen DISP)(%gen APPLY))))))))

(define (compile-bard expr #!optional (env '()))
  (cond
   ((%symbol? expr)(if (%keyword? expr)
                       (%gen CONST expr)
                       (%gen-var-ref expr env)))
   ((%nothing? expr)(%gen NOTHING))
   ((%list? expr)(%compile-application expr env))
   (else (%gen CONST expr))))

(define (assemble-instruction instr)
  (cons (opname->opcode (car instr))
        (cdr instr)))

(define (assemble-bard code)
  (list->vector (map assemble-instruction code)))

;;; (compile-bard 'x)
;;; (assemble-bard (compile-bard 'x))
;;; (compile-bard '())
;;; (assemble-bard (compile-bard '()))
;;; (compile-bard '(quote x))
;;; (assemble-bard (compile-bard '(quote x)))
;;; (compile-bard '(begin  1 2 3))
;;; (assemble-bard (compile-bard '(begin  1 2 3)))
;;; (compile-bard 3)
;;; (assemble-bard (compile-bard 3))

