;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          flist.scm
;;;; Project:       Bard
;;;; Purpose:       a flist is a pure-functional list represented as a finger tree
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type flist extender: define-flist)
(define-flist empty-flist)
(define-flist flist-single read-only: value)
(define-flist flist-tree read-only: lcount ldigit mcount mlist rcount rdigit)

(define flnil #f)
(let ((_nil (make-empty-flist)))
  (set! flnil (lambda () _nil)))

(define (flnull? x)(eq? x (flnil)))

(define (lmerge3 ls fl)
  (lcons (list-ref ls 0)
         (lcons (list-ref ls 1)
                (lcons (list-ref ls 2) fl))))

(define (rmerge3 fl ls)
  (rcons (rcons (rcons fl (list-ref ls 2))
                (list-ref ls 1))
         (list-ref ls 0)))

(define (lcons x fl)
  (cond
   ((flnull? fl)(make-flist-single x))
   ((flist-single? fl)(make-flist-tree 1 (list x) 0 (flnil) 1 (list (flist-single-value fl))))
   ((flist-tree? fl)(if (< (flist-tree-lcount fl) 4)
                        (make-flist-tree (+ 1 (flist-tree-lcount fl))
                                         (cons x (flist-tree-ldigit fl))
                                         (flist-tree-mcount fl)
                                         (flist-tree-mlist fl)
                                         (flist-tree-rcount fl)
                                         (flist-tree-rdigit fl))
                        (make-flist-tree 2
                                         (list x (car (flist-tree-ldigit fl)))
                                         (+ 3 (flist-tree-mcount fl))
                                         (lmerge3 (cdr (flist-tree-ldigit fl))
                                                  (flist-tree-mlist fl))
                                         (flist-tree-rcount fl)
                                         (flist-tree-rdigit fl))))
   (else (error "type error" fl))))

(define (rcons fl x) 
  (cond
   ((flnull? fl)(make-flist-single x))
   ((flist-single? fl)(make-flist-tree 1 (list (flist-single-value fl)) 0 (flnil) 1 (list x)))
   ((flist-tree? fl)(if (< (flist-tree-rcount fl) 4)
                        (make-flist-tree (flist-tree-lcount fl)
                                         (flist-tree-ldigit fl)
                                         (flist-tree-mcount fl)
                                         (flist-tree-mlist fl)
                                         (+ 1 (flist-tree-rcount fl))
                                         (cons x (flist-tree-rdigit fl)))
                        (make-flist-tree (flist-tree-lcount fl)
                                         (flist-tree-ldigit fl)
                                         (+ 3 (flist-tree-mcount fl))
                                         (rmerge3 (flist-tree-mlist fl)
                                                  (cdr (flist-tree-rdigit fl)))
                                         2
                                         (list x (car (flist-tree-rdigit fl))))))
   (else (error "type error" fl))))

(define (flength fl)
  (cond
   ((flnull? fl) 0)
   ((flist-single? fl) 1)
   ((flist-tree? fl)(+ (flist-tree-lcount fl)(flist-tree-mcount fl)(flist-tree-rcount fl)))
   (else (error "type error" fl))))


(define (flist-tree-element fl n)
  (let ((lct (flist-tree-lcount fl))
        (mct (flist-tree-mcount fl))
        (rct (flist-tree-rcount fl)))
    (cond
     ((< n lct)(list-ref (flist-tree-ldigit fl) n))
     ((< n (+ lct mct))(let ((m (flist-tree-mlist fl)))
                         (cond
                          ((flist-single? m) (flist-single-value fl))
                          ((flist-tree? m)(flist-tree-element m (- n lct)))
                          (else (error "type error" fl)))))
     ((< n (+ lct mct rct))(let ((digitlen rct)
                                 (digit (flist-tree-rdigit fl))
                                 (rind (- n (+ lct mct))))
                             (list-ref digit (- (- rct 1) rind))))
     (else (error "index out of range" n)))))

(define (flelement fl n)
  (cond
   ((flnull? fl) (error "index out of range" n))
   ((flist-single? fl) (if (zero? n)(flist-single-value fl)(error "index out of range" n)))
   ((flist-tree? fl)(flist-tree-element fl n))
   (else (error "type error" fl))))

(define (lcar fl) #f)
(define (rcar fl) #f)

(define (lcdr fl) #f)
(define (rcdr fl) #f)
