;;;; ***********************************************************************
;;;;
;;;; Name:          quasiquote.scm
;;;; Project:       Bard
;;;; Purpose:       the quasiquote expander
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; ABOUT
;;; ----------------------------------------------------------------------
;;; quasiquote, unquote and unquote-splicing exist only
;;; in the compiler; they're not in the kernel language

;;; (quasiquote foo)

;;; quasiquote
;;; ----------------------------------------------------------------------
;;; after norvig

(define (constant? exp)
  (if (pair? exp)
      (eq? (car exp) 'quote)
      (not (symbol? exp))))

(define (combine-skeletons left right exp)
  (cond
   ((and (constant? left) (constant? right)) 
    (if (and (eqv? (kernel:eval left) (car exp))
             (eqv? (kernel:eval right) (cdr exp)))
        (list 'quote exp)
        (list 'quote (cons (kernel:eval left) (kernel:eval right)))))
   ((null? right) (list 'list left))
   ((and (pair? right) (eq? (car right) 'list))
    (cons 'list (cons left (cdr right))))
   (else (list 'cons left right))))

(define (%expand-quasiquote exp nesting)
  (cond
   ((not (pair? exp)) 
    (if (constant? exp) exp (list 'quote exp)))
   ((and (eq? (car exp) 'unquote) (= (length exp) 2))
    (if (= nesting 0)
        (cadr exp)
        (combine-skeletons ''unquote 
                           (%expand-quasiquote (cdr exp) (- nesting 1))
                           exp)))
   ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
    (combine-skeletons ''quasiquote 
                       (%expand-quasiquote (cdr exp) (+ nesting 1))
                       exp))
   ((and (pair? (car exp))
         (eq? (caar exp) 'unquote-splicing)
         (= (length (car exp)) 2))
    (if (= nesting 0)
        (list 'append (cadr (car exp))
              (%expand-quasiquote (cdr exp) nesting))
        (combine-skeletons (%expand-quasiquote (car exp) (- nesting 1))
                           (%expand-quasiquote (cdr exp) nesting)
                           exp)))
   (else (combine-skeletons (%expand-quasiquote (car exp) nesting)
                            (%expand-quasiquote (cdr exp) nesting)
                            exp))))

