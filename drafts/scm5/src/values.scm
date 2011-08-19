;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       bard
;;;; Purpose:       built-in Bard types
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; functions defined in this file are representation-specific; they're
;;; named with prefixes that identify their representations.
;;; The normal programmer API uses generic functions, which resolve
;;; to the representation-specific functions through dispatch.
;;; each representation's Category in the standard Bard type graph is 
;;; listed in the following form:
;;; <representation>->Category

(##include "~~/lib/termite/termite#.scm")

;;; ---------------------------------------------------------------------
;;; <eof>->Eof
;;; ---------------------------------------------------------------------

(define (<eof>:eof) #!eof)
(define (<eof>:eof? x) (eqv? x #!eof))

;;; ---------------------------------------------------------------------
;;; <undefined>->Undefined
;;; ---------------------------------------------------------------------

(define (<undefined>:undefined) #!unbound)
(define (<undefined>:undefined? x) (eqv? x #!unbound))
(define (<undefined>:defined? x)(not (<undefined>:undefined? x)))

;;; ---------------------------------------------------------------------
;;; <nothing>->Nothing
;;; ---------------------------------------------------------------------

(define <nothing>:nothing #f)
(define <nothing>:nothing? #f)
(let ((%nothing (cons '() '())))
  (set! <nothing>:nothing (lambda () %nothing))
  (set! <nothing>:nothing? (lambda (x) (eq? x %nothing))))
(define (<nothing>:something? x)(not (<nothing>:nothing? x)))

;;; ---------------------------------------------------------------------
;;; <boolean>->Boolean
;;; ---------------------------------------------------------------------

(define (<boolean>:false) #f)
(define (<boolean>:false? x) 
  (or (eqv? x (<boolean>:false))
      (<nothing>:nothing? x)))

(define (<boolean>:true) #t)
(define (<boolean>:true? x) (not (<boolean>:false? x)))

(define (<boolean>:boolean? x)
  (or (eqv? x (<boolean>:false))
      (eqv? x (<boolean>:true))))

;;; ---------------------------------------------------------------------
;;; <ralist>->Sequence
;;; ---------------------------------------------------------------------

(define (<ralist>:empty) ra:null)
(define <ralist>:empty? ra:null?)
(define <ralist>:ralist? ra:list?)

(define (<ralist>:add-first x s)(ra:cons x s))
(define (<ralist>:ralist . args)(apply ra:list args))
(define (<ralist>:make elts)(ra:make-list elts))
(define (<ralist>:first s)(ra:car s))
(define (<ralist>:rest s)(ra:cdr s))
(define (<ralist>:second s)(ra:caar s))
(define (<ralist>:third s)(ra:caaar s))
(define (<ralist>:fourth s)(ra:caaaar s))
(define (<ralist>:length s)(ra:length s))
(define <ralist>:fold-left fold-left)
(define <ralist>:fold-right fold-right)
(define <ralist>:append ra:append)
(define <ralist>:reverse ra:reverse)
(define (<ralist>:drop n s)(ra:list-tail s n))
(define <ralist>:element ra:list-ref)
(define <ralist>:image ra:map)
(define (<ralist>->list ral)
  (let loop ((ral ral)
             (result '()))
    (if (<ralist>:empty? ral)
        (reverse result)
        (loop (<ralist>:rest ral)
              (cons (<ralist>:first ral)
                    result)))))

;;; ---------------------------------------------------------------------
;;; <cell>->Cell
;;; ---------------------------------------------------------------------

(define (%data-make-process-name type)
  (string->symbol 
    (string-append 
      (symbol->string 
        (thread-name 
          (current-thread))) 
      "-"
      (symbol->string type))))

;; ----------------------------------------------------------------------------
;; Cells

(define (<cell>:make #!key (name (%data-make-process-name 'cell))
                     #!rest content)
  (spawn
    (lambda ()
      (let loop ((content (if (pair? content)
                              (car content)
                              (void))))
        (recv
          ((from tag 'empty?)
           (! from (list tag (eq? (void) content)))
           (loop content))

          ((from tag 'ref)
           (! from (list tag content))
           (loop content))

          (('set! content)
           (loop content)))))
    name: name))

(define (<cell>:get cell)
  (!? cell 'ref))

(define (<cell>:put! cell value)
  (! cell (list 'set! value)))

(define (<cell>:empty? cell)
  (!? cell 'empty?))

(define (<cell>:cell? x)
  (thread? x))

;;; ---------------------------------------------------------------------
;;; <slot>->Slot
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

(define (<slot>:make key value) 
  (%make-slot key value))

(define (<slot>:slot? x)
  (bard:slot? x))

(define (<slot>:key x)
  (bard:slot-key x))

(define (<slot>:value x)
  (bard:slot-value x))

;;; ---------------------------------------------------------------------
;;; <frame>->Map
;;; ---------------------------------------------------------------------

(define <frame>:frame? #f)

;;; - comparisons -------------------------------------------------------

;;; TODO: extend this and make it generic once we have generic functions

(define (bard:value<? x y)
  (let ((x (if (<cell>:cell? x)
               (<cell>:get x)
               x))
        (y (if (<cell>:cell? y)
               (<cell>:get y)
               y)))
    (cond
     ((<undefined>:undefined? x)(if (<undefined>:undefined? y) #f #t))
     ((<nothing>:nothing? x)(cond
                             ((<undefined>:undefined? y) #f)
                             ((<nothing>:nothing? y) #f)
                             (else #t)))
     ((<eof>:eof? x)(cond
                     ((<undefined>:undefined? y) #f)
                     ((<nothing>:nothing? y) #f)
                     ((<eof>:eof? y) #f)
                     (else #t)))
     ((<boolean>:boolean? x)(cond
                             ((<undefined>:undefined? y) #f)
                             ((<nothing>:nothing? y) #f)
                             ((<eof>:eof? y) #f)
                             ((<boolean>:boolean? y)(if (<boolean>:false? x)
                                                        (if (<boolean>:false? y)
                                                            #f
                                                            #t)
                                                        #f))
                             (else #t)))
     ((char? x)(cond
                ((<undefined>:undefined? y) #f)
                ((<nothing>:nothing? y) #f)
                ((<eof>:eof? y) #f)
                ((<boolean>:boolean? y) #f)
                ((char? y)(char<? x y))
                (else #t)))
     ((number? x)(cond
                  ((<undefined>:undefined? y) #f)
                  ((<nothing>:nothing? y) #f)
                  ((<eof>:eof? y) #f)
                  ((<boolean>:boolean? y) #f)
                  ((char? y) #f)
                  ((number? y)(< x y))
                  (else #t)))
     ((symbol? x)(cond
                  ((<undefined>:undefined? y) #f)
                  ((<nothing>:nothing? y) #f)
                  ((<eof>:eof? y) #f)
                  ((<boolean>:boolean? y) #f)
                  ((char? y) #f)
                  ((number? y) #f)
                  ((symbol? y)(string<? (symbol->string x)(symbol->string y)))
                  (else #t)))
     ((keyword? x)(cond
                   ((<undefined>:undefined? y) #f)
                   ((<nothing>:nothing? y) #f)
                   ((<eof>:eof? y) #f)
                   ((<boolean>:boolean? y) #f)
                   ((char? y) #f)
                   ((number? y) #f)
                   ((symbol? y) #f)
                   ((keyword? y)(string<? (keyword->string x)(keyword->string y)))
                   (else #t)))
     ((string? x)(cond
                   ((<undefined>:undefined? y) #f)
                   ((<nothing>:nothing? y) #f)
                   ((<eof>:eof? y) #f)
                   ((<boolean>:boolean? y) #f)
                   ((char? y) #f)
                   ((number? y) #f)
                   ((symbol? y) #f)
                   ((keyword? y) #f)
                   ((string? y)(string<? x y))
                   (else #t)))
     ((<slot>:slot? x)(cond
                       ((<undefined>:undefined? y) #f)
                       ((<nothing>:nothing? y) #f)
                       ((<eof>:eof? y) #f)
                       ((<boolean>:boolean? y) #f)
                       ((char? y) #f)
                       ((number? y) #f)
                       ((symbol? y) #f)
                       ((keyword? y) #f)
                       ((string? y) #f)
                       ((<slot>:slot? y)(bard:value<? (<slot>:key x)(<slot>:key y)))
                       (else #t)))
     ((<ralist>:ralist? x)(cond
                           ((<undefined>:undefined? y) #f)
                           ((<nothing>:nothing? y) #f)
                           ((<eof>:eof? y) #f)
                           ((<boolean>:boolean? y) #f)
                           ((char? y) #f)
                           ((number? y) #f)
                           ((symbol? y) #f)
                           ((keyword? y) #f)
                           ((string? y) #f)
                           ((<slot>:slot? y) #f)
                           ((<ralist>:ralist? y)(cond
                                                 ((< (<ralist>:length x)
                                                     (<ralist>:length y)) #t)
                                                 ((> (<ralist>:length x)
                                                     (<ralist>:length y)) #f)
                                                 (else (let loop ((xs x)
                                                                  (ys y))
                                                         (if (<ralist>:empty? xs)
                                                             #f
                                                             (let ((x0 (<ralist>:first xs))
                                                                   (y0 (<ralist>:first ys)))
                                                               (if (bard:value<? x0 y0)
                                                                   #t
                                                                   (loop (<ralist>:rest xs)
                                                                         (<ralist>:rest ys)))))))))
                           (else #t)))
     ((<frame>:frame? x)(cond
                         ((<undefined>:undefined? y) #f)
                         ((<nothing>:nothing? y) #f)
                         ((<eof>:eof? y) #f)
                         ((<boolean>:boolean? y) #f)
                         ((char? y) #f)
                         ((number? y) #f)
                         ((symbol? y) #f)
                         ((keyword? y) #f)
                         ((string? y) #f)
                         ((<slot>:slot? y) #f)
                         ((<ralist>:ralist? y) #f)
                         ((<frame>:frame? y)(let ((xkeys (<frame>:keys x))
                                                  (ykeys (<frame>:keys y)))
                                              (bard:value<? xkeys ykeys)))
                         (else (error "Comparison is not defined for values of this type" y))))
     (else (error "Comparison is not defined for values of this type" x)))))

;;; - frames -------------------------------------------------------------

(define <frame>:frame (make-wt-tree-type bard:value<?))

(define <frame>:empty #f)
(define <frame>:empty? #f)
(let ((fr (alist->wt-tree <frame>:frame '())))
  (set! <frame>:empty (lambda () fr))
  (set! <frame>:empty? (lambda (x) (eq? x fr))))

(define (<frame>:frame? x)
  (and (wttree? x)
       (eq? <frame>:frame (wt-tree-type x))))

(define (<frame>:alist->frame alist)
  (alist->wt-tree <frame>:frame alist))

(define (<frame>:plist->frame plist)
  (<frame>:alist->frame (plist->alist plist)))

(define (<frame>:get m k #!key (default (<undefined>:undefined)))
  (wt-tree/lookup m k default))

(define (<frame>:put m k val)
  (wt-tree/add m k val))

(define (<frame>:keys m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index m i)
                      result))
          (apply <ralist>:ralist (reverse result))))))

(define (<frame>:vals m)
  (let ((keycount (wt-tree/size m)))
    (let loop ((i 0)
               (result '()))
      (if (< i keycount)
          (loop (+ i 1)
                (cons (wt-tree/index-datum m i)
                      result))
          (apply <ralist>:ralist (reverse result))))))

;;; later key/val pairs override earlier ones
(define (<frame>:%merge m1 m2)
  (wt-tree/union m1 m2))

(define (make-frame . inits)
  (<frame>:plist->frame inits))

