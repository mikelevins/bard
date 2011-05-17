;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.scm
;;;; Project:       bard
;;;; Purpose:       Bard sequence values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; sequences
;;; ---------------------------------------------------------------------

(define bard:sequence number-wt-type)

(define bard:empty-sequence #f)
(let ((%empty-seq (alist->wt-tree bard:sequence '())))
  (set! bard:empty-sequence (lambda () %empty-seq)))

(define (bard:sequence? x)
  (and (wt-tree? x)
       (eq? bard:sequence (wt-tree-type x))))

(define (bard:list->sequence vals)
  (let* ((indexes (range 0 (length vals)))
         (alist (zip indexes vals)))
    (alist->wt-tree bard:sequence alist)))

(define (bard:element seq n)
  (let* ((default (gensym))
         (val (wt-tree/lookup seq n default)))
    (if (eq? val default)
        (error "Index out of range" n)
        val)))

(define (bard:take n seq)
  (wt-tree/split< seq n))

(define (bard:drop n seq)
  (wt-tree/split> seq (- n 1)))

(define (bard:first seq)
  (wt-tree/index-datum seq 0))

(define (bard:rest seq)
  (drop 1 seq))

(define (bard:count seq)
  (wt-tree/size seq))

(define (bard:empty? seq)
  (< (bard:count seq) 1))

(define (bard:every? pred ss)
  (if (bard:empty? ss)
      #t
      (let ((firsts (map first ss))
            (rests (map rest ss)))
        (if (apply pred firsts)
            (apply (partial bard:every? pred) rests)
            #f))))
