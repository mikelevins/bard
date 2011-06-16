;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       representation of built-in Bard types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(##include "~~/lib/gambit#.scm")
(##include "~~/lib/termite/termite#.scm")
(##include "~~/lib/termite/data.scm")

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
(define (seq:make-sequence k . rest)(apply ra:make-list `(,k . ,@rest)))
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
;;; cells
;;; ---------------------------------------------------------------------

(define (bard:make-cell value)
  (make-cell value))

(define (bard:cell? thing)
  (thread? $c))

(define (bard:get-cell cell)
  (cell-ref cell))

(define (bard:put-cell! cell val)
  (cell-set! cell val))

(define (bard:cell-empty? cell)
  (cell-empty? cell))

;;; ---------------------------------------------------------------------
;;; slots
;;; ---------------------------------------------------------------------
;;; we normally never see bare slots; instead we see them as the
;;; output of accessing a frame as a sequence. frames are not
;;; necessarily represented as sequences of slots, but when treated as
;;; sequences, they are *logically* sequences of slots, and fetching
;;; an individual element of a frame returns a slot

(define-type bard:slot
  id: CD0F8C5C-BF98-4824-B9B9-28B0B72228AD
  constructor: %make-slot
  (key bard:slot-key)
  (value bard:slot-value))

(define (bard:slot key value) 
  (%make-slot key value))

;;; ---------------------------------------------------------------------
;;; frames
;;; ---------------------------------------------------------------------

;;; - comparisons -------------------------------------------------------

(define (bard:comparison-type x)
  (cond
   ((bard:undefined? x) 'undefined)
   ((bard:nothing? x) 'nothing)
   ((or (eqv? x (bard:true))(eqv? x (bard:false))) 'boolean)
   ((or (integer? x)(flonum? x)(##ratnum? x)) 'number)
   ((char? x) 'character)
   ((or (symbol? x)(keyword? x)) 'name)
   ((string? x) 'text)
   ((bard:sequence? x) 'sequence)))

(define (%name->string x)
  (cond
   ((symbol? x)(symbol->string x))
   ((keyword? x)(keyword->string x))
   (else (error "not a name" x))))

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
    ((text)(lambda (x y)(string<? x y)))
    ((name)(lambda (x y)(string<? (%name->string x)
                                  (%name->string y))))
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

;;; - frames -------------------------------------------------------------

(define frame:frame (make-wt-tree-type bard:value<?))

(define frame:empty #f)
(define frame:empty? #f)
(let ((fr (alist->wt-tree frame:frame '())))
  (set! frame:empty (lambda () fr))
  (set! frame:empty? (lambda (x) (eq? x fr))))

(define (frame:map? x)
  (and (wt-tree? x)
       (eq? frame:frame (wt-tree-type x))))

(define (frame:alist->frame alist)
  (alist->wt-tree frame:frame alist))

(define (frame:plist->frame plist)
  (frame:alist->frame (plist->alist plist)))

(define (frame:get m k #!key (default (bard:undefined)))
  (wt-tree/lookup m k default))

(define (frame:put m k val)
  (wt-tree/add m k val))

(define (frame:keys m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index m i)
                      result))
          (reverse result)))))

(define (frame:vals m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index-datum m i)
                      result))
          (reverse result)))))

;;; later key/val pairs override earlier ones
(define (frame:%merge m1 m2)
  (wt-tree/union m1 m2))

(define (make-frame . inits)
  (frame:plist->frame inits))