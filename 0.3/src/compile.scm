;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compile.scm
;;;; Project:       Bard 0.3
;;;; Purpose:       bard 0.3 compiler
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%nothing) '())

(define %null? null?)
(define %symbol? symbol?)
(define %list? pair?)
(define (%special-form? expr) #f)
(define (%macro-form? expr) #f)
(define (%sequence? x) #f)
(define (%table? x) #f)
(define (%function? x) #f)
(define (%method? x) #f)
(define (%primitive? x) #f)

(define (%gen op . args) `(,op ,@args))
(define (%gen-nothing) (%gen 'NIL))
(define (%gen-constant expr) (%gen 'VAL expr))
(define (%gen-apply-sequence op args) #f)
(define (%gen-apply-table op args) #f)
(define (%gen-apply-function op args) #f)
(define (%gen-apply-method op args) #f)
(define (%gen-apply-primitive op args) #f)

(define (%default-environment)(null-env))
(define (%bard-modules) '())

(define (%compile-variable expr env modules) 
  (receive (i j)(find-in-env expr env)
           (if (and i j)
               (%gen 'LREF i j)
               (receive (m v)(find-in-modules expr modules)
                        (if (and m v)
                            (%gen 'MREF m v)
                            (error (string-append "Compiler error: undefined variable: "
                                                  (object->string expr))))))))

(define (%compile-applicable expr env modules)
  (let* ((op (%compile (%car expr) env modules))
         (args (map (lambda (x)(%compile x env modules))
                    (%cdr expr))))
    (cond
     ((%sequence? op)(%gen-apply-sequence op args))
     ((%table? op)(%gen-apply-table op args))
     ((%function? op)(%gen-apply-function op args))
     ((%method? op)(%gen-apply-method op args))
     ((%primitive? op)(%gen-apply-primitive op args))
     (else (error (string-append "Compiler error: tried to apply something not recognized as applicable: "
                                 (object->string op)))))))

(define (%compile-application expr env modules) 
  (cond
   ((%special-form? expr)(%compile-special-form expr env modules))
   ((%macro-form? expr)(%compile (%expand-macro-form expr) env modules))
   (else (%compile-applicable expr env modules))))

(define (%compile expr #!optional (env (%default-environment))(modules (%bard-modules)))
  (cond
   ((%null? expr) (%gen-nothing))
   ((%symbol? expr) (%compile-variable expr env modules))
   ((%list? expr) (%compile-application expr env modules))
   (else (%gen-constant expr))))

;;; (define $modules (%initial-bard-registry))
;;; (define $env (null-env))
;;; (set! $env (extend-env $env (make-binding 'a "a")(make-binding 'b "b")))
;;; (set! $env (extend-env $env (make-binding 'c "c")(make-binding 'd "d")(make-binding 'e "e")))
;;; (%compile 5 $env $modules)
;;; (%compile (%nothing) $env $modules)
;;; (%compile 'a $env $modules)
;;; (%compile 'e $env $modules)
;;; (%compile 'bard.lang:*module* $env $modules)
;;; (%compile 'x $env $modules)

