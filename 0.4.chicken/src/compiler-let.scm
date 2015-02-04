;;;; ***********************************************************************
;;;;
;;;; Name:          compiler-let.scm
;;;; Project:       Bard
;;;; Purpose:       compilation of let forms
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; 
;;; ---------------------------------------------------------------------
;;; degenerate case:
;;; (let () x y z) => (begin x y z)
;;; simplest let:
;;; (let (a) x y z) => ((^ (a) x y z) nothing)
;;; single binding:
;;; (let ((a foo)) x y z) => ((^ (a) x y z) foo)
;;; single multiple-value binding:
;;; (let ((a b c (foo ...))) x y z) => (apply (^ (a b c) x y z) (foo ...))
;;; multiple bindings
;;; (let (a b c) x y z) => (let (a) (let (b) (let (c) x y z)))

(define (bard:expand-simple-let binding body)
  (if (symbol? binding)
      `((^ (,binding) ,@body) nothing)
      (let ((var (car binding))
            (val (cadr binding)))
        `((^ (,var) ,@body) ,val))))

(define (bard:expand-compound-let bindings body)
  (if (= 1 (length bindings))
      (bard:expand-simple-let (car bindings) body)
      (let ((first-binding (car bindings))
            (rest-bindings (cdr bindings)))
        `(let (,first-binding)
           ,(bard:expand-compound-let rest-bindings body)))))

(define (bard:compile-let expr env)
  (let ((bindings (list-ref expr 1))
        (body (drop 2 expr)))
    (if (list? bindings)
        (if (null? bindings)
            (bard:compile `(begin ,@body) env)
            (bard:compile (bard:expand-compound-let bindings body) env))
        (error "The first argument to let must be a list of binding forms"))))

