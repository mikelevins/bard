;;; Procedures for implementing SRFI-25 arrays.

(require <kawa.lib.prim_syntax>)

(define-alias <array>  <gnu.lists.Array>)

(define (array? x) :: <boolean>
  (instance? x <array>))

(define (shape #!rest (args :: <Object[]>)) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'shape args))

(define (make-array (shape :: <array>) #!optional (obj #!null)) :: <array>
  (invoke-static <gnu.kawa.functions.Arrays> 'make shape obj))

(define (array (shape :: <array>) #!rest (vals :: <Object[]>))
  (invoke-static <gnu.kawa.functions.Arrays> 'makeSimple
		 shape (gnu.lists.FVector vals)))

(define (array-rank (array :: <array>)) :: <int>
  (invoke array 'rank))

(define (array-start (array :: <array>) (k :: <int>)) :: <int>
  (invoke array 'getLowBound k))

(define (array-end  (array :: <array>) (k :: <int>)) :: <int>
  (+ (invoke array 'getLowBound k) (invoke array 'getSize k)))

(define (share-array (array :: <array>) (shape :: <array>)
		     (mapper :: <procedure>))
  (invoke-static  <gnu.kawa.functions.Arrays> 'shareArray array shape mapper))
