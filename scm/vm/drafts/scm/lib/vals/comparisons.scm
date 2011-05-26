;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          comparisons.scm
;;;; Project:       bard
;;;; Purpose:       type-independent ordering
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; comparisons
;;; ---------------------------------------------------------------------

(define (bard:comparison-type x)
  (cond
   ((bard:undefined? x) 'undefined)
   ((bard:nothing? x) 'nothing)
   ((or (eqv? x (bard:true))(eqv? x (bard:false))) 'boolean)
   ((or (bard:integer? x)(bard:float? x)(bard:ratio? x)) 'number)
   ((bard:character? x) 'character)
   ((bard:sequence? x) 'sequence)))

(define (bard:comparison-for-type tp)
  (case tp
    ((undefined)(lambda (x y) #f))
    ((nothing)(lambda (x y) #f))
    ((boolean)(lambda (x y)(if (eqv? x (bard:false))
                               (if (eqv? y (bard:true))
                                   #t
                                   #f)
                               #f)))
    ((number)(lambda (x y)(< x y)))
    ((character)(lambda (x y)(char<? x y)))
    ((sequence)(lambda (x y)(cond
                             ((< (count x)(count y)) #t)
                             ((= (count x)(count y))(bard:every? bard:value<? x y))
                             (else #f))))))

(define $bard-type-comparison-order
  '(undefined nothing boolean number character sequence))

(define (bard:compare-by-type x y)
  (< (position x $bard-type-comparison-order eqv?)
     (position y $bard-type-comparison-order eqv?)))

(define (bard:same-type? x y)
  (eqv? (bard:comparison-type x)
        (bard:comparison-type y)))

(define (bard:value<? x y)
  (if (bard:same-type? x y)
      ((bard:comparison-for-type (bard:comparison-type x)) x y)
      (bard:compare-by-type x y)))
