;;; notes on define-function syntax

;;; unspecialized (default method)

(define-function add (x y)
  (error "Don't know how to add the arguments ~s and ~s" x y))

;;; type-specialized

(define-function add ((x Number) (y Number))
  (+ x y))

(define-function add ((x Sequence) (y Sequence))
  (concat x y))

;;; either categories or representations may be used for specialization

(define-function add ((x <object-vector>) (y <object-vector>))
  (concat x y))

;;; equal specialized (more specific than type-specialized)

(define-function add (x (y == nothing))
  (sequence x))

;;; predicate specialized
;;; a Dispatch Predicate is a generic function that is applied to the
;;; parameters bound in the lambda list.  If it returns true, then the
;;; method is applicable.  Dispatch Predicates are categories and may
;;; be composed in taxonomies with other categories; taxonomies are
;;; used to resolve otherwise-ambiguous method dispatch

(define-function add (x y) ?? MonotonicallyIncreasing?
  (sequence x y))

;;; the general order of specificity is, in order of increasing specificity:
;;; unspecialized > type-specialized > equal-specialized > predicate-specialized



