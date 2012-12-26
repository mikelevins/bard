;;;============================================================================

;;; File: "Sort.scm", Time-stamp: <2008-03-18 15:21:35 feeley>

;;; Copyright (c) 2006-2008 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; (sort sequence less?) sorts a sequence (a list or vector) in a
;;; non-destructive way ordered using the comparison predicate less?.
;;;
;;; Sample use:
;;;
;;;  (sort (vector 3 1 4 1 5) >) => #(5 4 3 1 1)

;;;============================================================================

(define (sort sequence less?)

  (declare (standard-bindings) (not safe))

  (define (sort-list lst less?)

    (define (mergesort lst)

      (define (merge lst1 lst2)
        (cond ((not (pair? lst1))
               lst2)
              ((not (pair? lst2))
               lst1)
              (else
               (let ((e1 (car lst1)) (e2 (car lst2)))
                 (if (less? e1 e2)
                     (cons e1 (merge (cdr lst1) lst2))
                     (cons e2 (merge lst1 (cdr lst2))))))))

      (define (split lst)
        (if (or (not (pair? lst)) (not (pair? (cdr lst))))
            lst
            (cons (car lst) (split (cddr lst)))))

      (if (or (not (pair? lst)) (not (pair? (cdr lst))))
          lst
          (let* ((lst1 (mergesort (split lst)))
                 (lst2 (mergesort (split (cdr lst)))))
            (merge lst1 lst2))))

    (mergesort lst))

  (cond ((not (procedure? less?))
         (error "procedure expected"))
        ((or (null? sequence)
             (pair? sequence))
         (sort-list sequence less?))
        ((vector? sequence)
         (list->vector (sort-list (vector->list sequence) less?)))
        (else
         (error "vector or list expected"))))

;;;============================================================================
