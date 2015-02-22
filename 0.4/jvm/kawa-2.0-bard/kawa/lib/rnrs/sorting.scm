(module-name <kawa.lib.rnrs.sorting>)
(module-export list-sort vector-sort vector-sort!)
;;(import (srfi :95 sorting-and-merging))
;(import (kawa lib srfi95))
(require <kawa.lib.srfi95>)

(define (list-sort less? list)
  (%sort-list (append list '()) less? #f))

(define (vector-sort less? seq)
  (%sort-vector seq less? #f))

(define (vector-sort! proc vector) :: void
  (%vector-sort! vector proc #f))
