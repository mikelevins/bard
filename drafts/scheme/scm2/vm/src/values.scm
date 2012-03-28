;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       representations of base value types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; eof
;;; ---------------------------------------------------------------------

(define (bard:eof) #!eof)
(define (bard:eof? x) (eqv? x #!eof))

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(define (bard:undefined) #!unbound)
(define (bard:undefined? x) (eqv? x #!unbound))
(define (bard:defined? x)(not (bard:undefined? x)))

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(define bard:nothing #f)
(define bard:nothing? #f)
(let ((%nothing (cons '() '())))
  (set! bard:nothing (lambda () %nothing))
  (set! bard:nothing? (lambda (x) (eq? x %nothing))))
(define (bard:something? x)(not (bard:nothing? x)))

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(define (bard:false) #f)
(define (bard:false? x) 
  (or (eqv? x (bard:false))
      (bard:nothing? x)))

(define (bard:true) #t)
(define (bard:true? x) (not (bard:false? x)))

;;; ---------------------------------------------------------------------
;;; sequences
;;; ---------------------------------------------------------------------

(define (bard:empty-sequence)(ra:null))
(define bard:empty-sequence? ra:null?)
(define bard:sequence? ra:list?)

(define (seq:add-first x s)(ra:cons x s))
(define (seq:sequence . args)(apply ra:list args))
(define (seq:make-sequence k .rest)(apply ra:make-list `(,k . ,rest)))
(define (seq:first s)(ra:car s))
(define (seq:rest s)(ra:cdr s))
(define (seq:second s)(ra:caar s))
(define (seq:third s)(ra:caaar s))
(define (seq:fourth s)(ra:caaaar s))
(define (seq:length s)(ra:length s))
(define seq:fold-left fold-left)
(define seq:fold-right fold-right)
(define seq:append ra:append)
(define seq:reverse ra:reverse)
(define (seq:drop n s)(ra:list-tail s n))
(define seq:element ra:list-ref)
(define seq:image ra:map)

;;; ---------------------------------------------------------------------
;;; maps
;;; ---------------------------------------------------------------------

;;; - comparisons -------------------------------------------------------

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

;;; - maps -------------------------------------------------------------

(define map:map (make-wt-tree-type bard:value<?))

(define map:empty-map #f)
(define map:empty-map? #f)
(let ((0map (alist->wt-tree map:map '())))
  (set! map:empty-map (lambda () 0map))
  (set! map:empty-map? (lambda (x) (eq? x 0map))))

(define (map:map? x)
  (and (wt-tree? x)
       (eq? map:map (wt-tree-type x))))

(define (map:alist->map alist)
  (alist->wt-tree map:map alist))

(define (map:plist->map plist)
  (map:alist->map (plist->alist plist)))

(define (map:get m k #!key (default #f))
  (wt-tree/lookup m k default))

(define (map:put m k val)
  (wt-tree/add m k val))

(define (map:keys m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index m i)
                      result))
          (reverse result)))))

(define (map:vals m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index-datum m i)
                      result))
          (reverse result)))))