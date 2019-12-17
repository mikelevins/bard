;;; "wttree.scm" Weight balanced trees -*-Scheme-*-
;;;
;;; Copyright (c) 2010 Kazu Yamamoto
;;;
;;; Reference:
;;;
;;; Yoichi Hirai and Kazuhiko Yamamoto,
;;; "Balance Condition on Weight-Balanced Trees",
;;; submitted to Journal of Functional Programming,
;;; 2010
;;;
;;; Copyright (c) 1993-1994 Stephen Adams
;;;
;;; References:
;;;
;;; Stephen Adams, Implemeting Sets Efficiently in a Functional
;;; Language, CSTR 92-10, Department of Electronics and Computer
;;; Science, University of Southampton, 1992
;;;
;;;
;;; Copyright (c) 1993-94 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science. Permission to copy and modify
;;; this software, to redistribute either the original software or a
;;; modified version, and to use this software for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warranty or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Weight Balanced Binary Trees
;;
;;
;;
;; This file has been modified from the MIT-Scheme library version to
;; make it more standard. The main changes are
;;
;; . The whole thing has been put in a LET as R4RS Scheme has no module
;; system.
;; . The MIT-Scheme define structure operations have been written out by
;; hand.
;;
;; It has been tested on MIT-Scheme, scheme48 and scm4e1
;;
;; If your system has a compiler and you want this code to run fast, you
;; should do whatever is necessary to inline all of the structure accessors.
;;
;; This is MIT-Scheme's way of saying that +, car etc should all be inlined.
;;
;;(declare (usual-integrations))

;;;
;;; Interface to this package.
;;;
;;; ONLY these procedures (and TEST at the end of the file) will be
;;; (re)defined in your system.
;;;
;@
(define make-wt-tree-type #f)
(define number-wt-type #f)
(define string-wt-type #f)
;@
(define make-wt-tree #f)
(define wttree? #f) ;;; added to API mle -- 2011-07-11
(define wt-tree-type #f) ;;; added to API mle -- 2011-07-11
(define singleton-wt-tree #f)
(define alist->wt-tree #f)
(define wt-tree/empty? #f)
(define wt-tree/size #f)
(define wt-tree/add #f)
(define wt-tree/delete #f)
(define wt-tree/add! #f)
(define wt-tree/delete! #f)
(define wt-tree/member? #f)
(define wt-tree/lookup #f)
(define wt-tree/split< #f)
(define wt-tree/split> #f)
(define wt-tree/union #f)
(define wt-tree/intersection #f)
(define wt-tree/difference #f)
(define wt-tree/subset? #f)
(define wt-tree/set-equal? #f)
(define wt-tree/fold #f)
(define wt-tree/for-each #f)
(define wt-tree/index #f)
(define wt-tree/index-datum #f)
(define wt-tree/index-pair #f)
(define wt-tree/rank #f)
(define wt-tree/min #f)
(define wt-tree/min-datum #f)
(define wt-tree/min-pair #f)
(define wt-tree/delete-min #f)
(define wt-tree/delete-min! #f)
(define wt-tree/valid? #f)

;; This LET sets all of the above variables.

(let ()

  ;; We use the folowing MIT-Scheme operation on fixnums (small
  ;; integers). R4RS compatible (but less efficient) definitions.
  ;; You should replace these with something that is efficient in your
  ;; system.

  (define fix:fixnum? (lambda (x) (and (exact? x) (integer? x))))
  (define fix:+ +)
  (define fix:- -)
  (define fix:< <)
  (define fix:<= <=)
  (define fix:> >)
  (define fix:* *)

  ;; A TREE-TYPE is a collection of those procedures that depend on the
  ;; ordering relation.

  ;; MIT-Scheme structure definition
  ;;(define-structure
  ;; (tree-type
  ;; (conc-name tree-type/)
  ;; (constructor %make-tree-type))
  ;; (key<? #F read-only true)
  ;; (alist->tree #F read-only true)
  ;; (add #F read-only true)
  ;; (insert! #F read-only true)
  ;; (delete #F read-only true)
  ;; (delete! #F read-only true)
  ;; (member? #F read-only true)
  ;; (lookup #F read-only true)
  ;; (split-lt #F read-only true)
  ;; (split-gt #F read-only true)
  ;; (union #F read-only true)
  ;; (intersection #F read-only true)
  ;; (difference #F read-only true)
  ;; (subset? #F read-only true)
  ;; (rank #F read-only true)
  ;;)

  ;; Written out by hand, using vectors:
  ;;
  ;; If possible, you should teach your system to print out something
  ;; like #[tree-type <] instread of the whole vector.

  (define tag:tree-type (string->symbol "#[(runtime wttree)tree-type]"))

  (define (%make-tree-type key<? alist->tree
                           add insert!
                           delete delete!
                           member? lookup
                           split-lt split-gt
                           union intersection
                           difference subset?
                           rank )
    (vector tag:tree-type
            key<? alist->tree add insert!
            delete delete! member? lookup
            split-lt split-gt union intersection
            difference subset? rank ))

  (define (tree-type? tt)
    (and (vector? tt)
         (eq? (vector-ref tt 0) tag:tree-type)))

  (define (tree-type/key<? tt) (vector-ref tt 1))
  (define (tree-type/alist->tree tt) (vector-ref tt 2))
  (define (tree-type/add tt) (vector-ref tt 3))
  (define (tree-type/insert! tt) (vector-ref tt 4))
  (define (tree-type/delete tt) (vector-ref tt 5))
  (define (tree-type/delete! tt) (vector-ref tt 6))
  (define (tree-type/member? tt) (vector-ref tt 7))
  (define (tree-type/lookup tt) (vector-ref tt 8))
  (define (tree-type/split-lt tt) (vector-ref tt 9))
  (define (tree-type/split-gt tt) (vector-ref tt 10))
  (define (tree-type/union tt) (vector-ref tt 11))
  (define (tree-type/intersection tt) (vector-ref tt 12))
  (define (tree-type/difference tt) (vector-ref tt 13))
  (define (tree-type/subset? tt) (vector-ref tt 14))
  (define (tree-type/rank tt) (vector-ref tt 15))

  ;; User level tree representation.
  ;;
  ;; WT-TREE is a wrapper for trees of nodes.
  ;;
  ;;MIT-Scheme:
  ;;(define-structure
  ;; (wt-tree
  ;; (conc-name tree/)
  ;; (constructor %make-wt-tree))
  ;; (type #F read-only true)
  ;; (root #F read-only false))

  ;; If possible, you should teach your system to print out something
  ;; like #[wt-tree] instread of the whole vector.

  (define tag:wt-tree (string->symbol "#[(runtime wttree)wt-tree]"))

  (define (%make-wt-tree type root)
    (vector tag:wt-tree type root))

  (define (wt-tree? t)
    (and (vector? t)
         (eq? (vector-ref t 0) tag:wt-tree)))

  (define (tree/type t) (vector-ref t 1))
  (define (tree/root t) (vector-ref t 2))
  (define (set-tree/root! t v) (vector-set! t 2 v))

  ;; Nodes are the thing from which the real trees are built. There are
  ;; lots of these and the uninquisitibe user will never see them, so
  ;; they are represented as untagged to save the slot that would be
  ;; used for tagging structures.
  ;; In MIT-Scheme these were all DEFINE-INTEGRABLE

  (define (make-node k v l r w) (vector w l k r v))
  (define (node/k node) (vector-ref node 2))
  (define (node/v node) (vector-ref node 4))
  (define (node/l node) (vector-ref node 1))
  (define (node/r node) (vector-ref node 3))
  (define (node/w node) (vector-ref node 0))

  (define empty 'empty)
  (define (empty? x) (eq? x 'empty))

  (define (node/size node)
    (if (empty? node) 0 (node/w node)))

  (define (node/singleton k v) (make-node k v empty empty 1))

  (define (with-n-node node receiver)
    (receiver (node/k node) (node/v node) (node/l node) (node/r node)))

  ;;
  ;; Constructors for building node trees of various complexity
  ;;

  (define (n-join k v l r)
    (make-node k v l r (fix:+ 1 (fix:+ (node/size l) (node/size r)))))

  (define (single-l a_k a_v x r)
    (with-n-node r
      (lambda (b_k b_v y z) (n-join b_k b_v (n-join a_k a_v x y) z))))

  (define (double-l a_k a_v x r)
    (with-n-node r
      (lambda (c_k c_v r_l z)
        (with-n-node r_l
          (lambda (b_k b_v y1 y2)
            (n-join b_k b_v
                    (n-join a_k a_v x y1)
                    (n-join c_k c_v y2 z)))))))

  (define (single-r b_k b_v l z)
    (with-n-node l
      (lambda (a_k a_v x y) (n-join a_k a_v x (n-join b_k b_v y z)))))

  (define (double-r c_k c_v l z)
    (with-n-node l
      (lambda (a_k a_v x l_r)
        (with-n-node l_r
          (lambda (b_k b_v y1 y2)
            (n-join b_k b_v
                    (n-join a_k a_v x y1)
                    (n-join c_k c_v y2 z)))))))

  ;; (define-integrable wt-tree-ratio 5)
  (define wt-tree-delta 3)
  (define wt-tree-gamma 2)

  (define (t-join k v l r)
    (define (simple-join) (n-join k v l r))
    (let ((l_n (fix:+ (node/size l) 1))
          (r_n (fix:+ (node/size r) 1)))
      (cond ((fix:> r_n (fix:* wt-tree-delta l_n))
             ;; right is too big
             (let ((r_l_n (fix:+ (node/size (node/l r)) 1))
                   (r_r_n (fix:+ (node/size (node/r r)) 1)))
               (if (fix:< r_l_n (fix:* wt-tree-gamma r_r_n))
                   (single-l k v l r)
                   (double-l k v l r))))
            ((fix:> l_n (fix:* wt-tree-delta r_n))
             ;; left is too big
             (let ((l_l_n (fix:+ (node/size (node/l l)) 1))
                   (l_r_n (fix:+ (node/size (node/r l)) 1)))
               (if (fix:< l_r_n (fix:* wt-tree-gamma l_l_n))
                   (single-r k v l r)
                   (double-r k v l r))))
            (else
             (simple-join)))))
  ;;
  ;; Node tree procedures that are independent of key<?
  ;;

  (define (node/min node)
    (cond ((empty? node) (error:empty 'min))
	  ((empty? (node/l node)) node)
	  (else (node/min (node/l node)))))

  (define (node/delmin node)
    (cond ((empty? node) (error:empty 'delmin))
          ((empty? (node/l node)) (node/r node))
          (else (t-join (node/k node) (node/v node)
			(node/delmin (node/l node)) (node/r node)))))

  (define (node/concat2 node1 node2)
    (cond ((empty? node1) node2)
          ((empty? node2) node1)
          (else
           (let ((min-node (node/min node2)))
             (t-join (node/k min-node) (node/v min-node)
                     node1 (node/delmin node2))))))

  (define (node/inorder-fold procedure base node)
    (define (fold base node)
      (if (empty? node)
          base
          (with-n-node node
            (lambda (k v l r)
              (fold (procedure k v (fold base r)) l)))))
    (fold base node))

  (define (node/for-each procedure node)
    (if (not (empty? node))
        (with-n-node node
          (lambda (k v l r)
            (node/for-each procedure l)
            (procedure k v)
            (node/for-each procedure r)))))

  (define (node/height node)
    (if (empty? node)
        0
        (+ 1 (max (node/height (node/l node))
                  (node/height (node/r node))))))

  (define (node/index node index)
    (define (loop node index)
      (let ((size_l (node/size (node/l node))))
        (cond ((fix:< index size_l) (loop (node/l node) index))
              ((fix:> index size_l) (loop (node/r node)
					  (fix:- index (fix:+ 1 size_l))))
              (else node))))
    (let ((bound (node/size node)))
      (if (or (< index 0)
              (>= index bound)
              (not (fix:fixnum? index)))
          (slib:error 'bad-range-argument index 'node/index)
          (loop node index))))

  (define (error:empty owner)
    (slib:error "Operation requires non-empty tree:" owner))


  (define (local:make-wt-tree-type key<?)

    ;; MIT-Scheme definitions:
    ;;(declare (integrate key<?))
    ;;(define-integrable (key>? x y) (key<? y x))

    (define (key>? x y) (key<? y x))

    (define (node/find k node)
      ;; Returns either the node or #f.
      ;; Loop takes D comparisons where D is the depth of the tree
      ;; rather than the traditional compare-low, compare-high which
      ;; takes on average 1.5(D-1) comparisons
      (define (loop this best)
        (cond ((empty? this) best)
              ((key<? k (node/k this)) (loop (node/l this) best))
              (else (loop (node/r this) this))))
      (let ((best (loop node #f)))
        (cond ((not best) #f)
              ((key<? (node/k best) k) #f)
              (else best))))

    (define (node/rank k node rank)
      (cond ((empty? node) #f)
            ((key<? k (node/k node)) (node/rank k (node/l node) rank))
            ((key>? k (node/k node))
             (node/rank k (node/r node)
                        (fix:+ 1 (fix:+ rank (node/size (node/l node))))))
            (else (fix:+ rank (node/size (node/l node))))))

    (define (node/add node k v)
      (if (empty? node)
          (node/singleton k v)
          (with-n-node node
            (lambda (key val l r)
              (cond ((key<? k key) (t-join key val (node/add l k v) r))
                    ((key<? key k) (t-join key val l (node/add r k v)))
                    (else (n-join key v l r)))))))

    (define (node/delete x node)
      (if (empty? node)
          empty
          (with-n-node node
            (lambda (key val l r)
              (cond ((key<? x key) (t-join key val (node/delete x l) r))
                    ((key<? key x) (t-join key val l (node/delete x r)))
                    (else (node/concat2 l r)))))))

    (define (node/concat tree1 tree2)
      (cond ((empty? tree1) tree2)
            ((empty? tree2) tree1)
            (else
             (let ((min-node (node/min tree2)))
               (node/concat3 (node/k min-node) (node/v min-node) tree1
                             (node/delmin tree2))))))

    (define (node/concat3 k v l r)
      (cond ((empty? l) (node/add r k v))
            ((empty? r) (node/add l k v))
            (else
             (let ((n1 (fix:+ (node/size l) 1))
                   (n2 (fix:+ (node/size r) 1)))
               (cond ((fix:< (fix:* wt-tree-delta n1) n2)
                      (with-n-node r
                        (lambda (k2 v2 l2 r2)
                          (t-join k2 v2 (node/concat3 k v l l2) r2))))
                     ((fix:< (fix:* wt-tree-delta n2) n1)
                      (with-n-node l
                        (lambda (k1 v1 l1 r1)
                          (t-join k1 v1 l1 (node/concat3 k v r1 r)))))
                     (else
                      (n-join k v l r)))))))

    (define (node/split-lt node x)
      (cond ((empty? node) empty)
            ((key<? x (node/k node))
             (node/split-lt (node/l node) x))
            ((key<? (node/k node) x)
             (node/concat3 (node/k node) (node/v node) (node/l node)
                           (node/split-lt (node/r node) x)))
            (else (node/l node))))

    (define (node/split-gt node x)
      (cond ((empty? node) empty)
            ((key<? (node/k node) x)
             (node/split-gt (node/r node) x))
            ((key<? x (node/k node))
             (node/concat3 (node/k node) (node/v node)
                           (node/split-gt (node/l node) x) (node/r node)))
            (else (node/r node))))

    (define (node/union tree1 tree2)
      (cond ((empty? tree1) tree2)
            ((empty? tree2) tree1)
            (else
             (with-n-node tree2
               (lambda (ak av l r)
                 (let ((l1 (node/split-lt tree1 ak))
                       (r1 (node/split-gt tree1 ak)))
                   (node/concat3 ak av (node/union l1 l) (node/union r1 r))))))))

    (define (node/difference tree1 tree2)
      (cond ((empty? tree1) empty)
            ((empty? tree2) tree1)
            (else
             (with-n-node tree2
               (lambda (ak av l r)
                 (let ((l1 (node/split-lt tree1 ak))
                       (r1 (node/split-gt tree1 ak)))
                   av
                   (node/concat (node/difference l1 l)
                                (node/difference r1 r))))))))

    (define (node/intersection tree1 tree2)
      (cond ((empty? tree1) empty)
            ((empty? tree2) empty)
            (else
             (with-n-node tree2
               (lambda (ak av l r)
                 (let ((l1 (node/split-lt tree1 ak))
                       (r1 (node/split-gt tree1 ak)))
                   (if (node/find ak tree1)
                       (node/concat3 ak av (node/intersection l1 l)
                                     (node/intersection r1 r))
                       (node/concat (node/intersection l1 l)
                                    (node/intersection r1 r)))))))))

    (define (node/subset? tree1 tree2)
      (or (empty? tree1)
          (and (fix:<= (node/size tree1) (node/size tree2))
               (with-n-node tree1
                 (lambda (k v l r)
                   v
                   (cond ((key<? k (node/k tree2))
                          (and (node/subset? l (node/l tree2))
                               (node/find k tree2)
                               (node/subset? r tree2)))
                         ((key>? k (node/k tree2))
                          (and (node/subset? r (node/r tree2))
                               (node/find k tree2)
                               (node/subset? l tree2)))
                         (else
                          (and (node/subset? l (node/l tree2))
                               (node/subset? r (node/r tree2))))))))))


;;; Tree interface: stripping off or injecting the tree types

    (define (tree/map-add tree k v)
      (%make-wt-tree (tree/type tree)
                     (node/add (tree/root tree) k v)))

    (define (tree/insert! tree k v)
      (set-tree/root! tree (node/add (tree/root tree) k v)))

    (define (tree/delete tree k)
      (%make-wt-tree (tree/type tree)
                     (node/delete k (tree/root tree))))

    (define (tree/delete! tree k)
      (set-tree/root! tree (node/delete k (tree/root tree))))

    (define (tree/split-lt tree key)
      (%make-wt-tree (tree/type tree)
                     (node/split-lt (tree/root tree) key)))

    (define (tree/split-gt tree key)
      (%make-wt-tree (tree/type tree)
                     (node/split-gt (tree/root tree) key)))

    (define (tree/union tree1 tree2)
      (%make-wt-tree (tree/type tree1)
                     (node/union (tree/root tree1) (tree/root tree2))))

    (define (tree/intersection tree1 tree2)
      (%make-wt-tree (tree/type tree1)
                     (node/intersection (tree/root tree1) (tree/root tree2))))

    (define (tree/difference tree1 tree2)
      (%make-wt-tree (tree/type tree1)
                     (node/difference (tree/root tree1) (tree/root tree2))))

    (define (tree/subset? tree1 tree2)
      (node/subset? (tree/root tree1) (tree/root tree2)))

    (define (alist->tree alist)
      (define (loop alist node)
        (cond ((null? alist) node)
              ((pair? alist) (loop (cdr alist)
				   (node/add node (caar alist) (cdar alist))))
              (else
               (slib:error 'wrong-type-argument alist "alist" 'alist->tree))))
      (%make-wt-tree my-type (loop alist empty)))

    (define (tree/get tree key default)
      (let ((node (node/find key (tree/root tree))))
        (if node
            (node/v node)
            default)))

    (define (tree/rank tree key) (node/rank key (tree/root tree) 0))

    (define (tree/member? key tree)
      (and (node/find key (tree/root tree))
           #t))

    (define my-type #f)

    (set! my-type
          (%make-tree-type
           key<?			; key<?
           alist->tree			; alist->tree
           tree/map-add			; add
           tree/insert!			; insert!
           tree/delete			; delete
           tree/delete!			; delete!
           tree/member?			; member?
           tree/get			; lookup
           tree/split-lt		; split-lt
           tree/split-gt		; split-gt
           tree/union			; union
           tree/intersection		; intersection
           tree/difference		; difference
           tree/subset?			; subset?
           tree/rank			; rank
           ))

    my-type)

  (define (guarantee-tree tree procedure)
    (if (not (wt-tree? tree))
        (slib:error 'wrong-type-argument
		    tree "weight-balanced tree" procedure)))

  (define (guarantee-tree-type type procedure)
    (if (not (tree-type? type))
        (slib:error 'wrong-type-argument
		    type "weight-balanced tree type" procedure)))

  (define (guarantee-compatible-trees tree1 tree2 procedure)
    (guarantee-tree tree1 procedure)
    (guarantee-tree tree2 procedure)
    (if (not (eq? (tree/type tree1) (tree/type tree2)))
        (slib:error "The trees" tree1 'and tree2 'have 'incompatible 'types
		    (tree/type tree1) 'and (tree/type tree2))))

  (define (valid? tree)
    (let ((root (tree/root tree)))
      (and (balanced? root)
	   (ordered? root))))

  (define (balanced? n)
    (define (isBalanced a b)
      (let ((x (fix:+ (node/size a) 1))
	    (y (fix:+ (node/size b) 1)))
	(fix:<= y (fix:* wt-tree-delta x))))
    (or (empty? n)
	(let ((l (node/l n))
	      (r (node/r n)))
	  (and (isBalanced l r) (isBalanced r l)
	       (balanced? l) (balanced? r)))))

  (define (ordered? n)
    (define (isOrdered lo hi m)
      (if (empty? m)
	  #t
	  (let ((k (node/k m))
		(l (node/l m))
		(r (node/r m)))
	    (and (lo k) (hi k)
		 (isOrdered lo (lambda (x) (< x k)) l)
		 (isOrdered (lambda (x) (< k x)) hi r)))))
    (isOrdered (lambda (x) #t) (lambda (x) #t) n))

;;;______________________________________________________________________
;;;
;;; Export interface
;;;
  (set! make-wt-tree-type local:make-wt-tree-type)

  (set! make-wt-tree
        (lambda (tree-type)
          (%make-wt-tree tree-type empty)))

  (set! wttree?
        (lambda (x)
          (wt-tree? x)))

  (set! wt-tree-type
        (lambda (x)
          (tree/type x)))

  (set! singleton-wt-tree
        (lambda (type key value)
          (guarantee-tree-type type 'singleton-wt-tree)
          (%make-wt-tree type (node/singleton key value))))

  (set! alist->wt-tree
        (lambda (type alist)
          (guarantee-tree-type type 'alist->wt-tree)
          ((tree-type/alist->tree type) alist)))

  (set! wt-tree/empty?
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/empty?)
          (empty? (tree/root tree))))

  (set! wt-tree/size
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/size)
          (node/size (tree/root tree))))

  (set! wt-tree/add
        (lambda (tree key datum)
          (guarantee-tree tree 'wt-tree/add)
          ((tree-type/add (tree/type tree)) tree key datum)))

  (set! wt-tree/delete
        (lambda (tree key)
          (guarantee-tree tree 'wt-tree/delete)
          ((tree-type/delete (tree/type tree)) tree key)))

  (set! wt-tree/add!
        (lambda (tree key datum)
          (guarantee-tree tree 'wt-tree/add!)
          ((tree-type/insert! (tree/type tree)) tree key datum)))

  (set! wt-tree/delete!
        (lambda (tree key)
          (guarantee-tree tree 'wt-tree/delete!)
          ((tree-type/delete! (tree/type tree)) tree key)))

  (set! wt-tree/member?
        (lambda (key tree)
          (guarantee-tree tree 'wt-tree/member?)
          ((tree-type/member? (tree/type tree)) key tree)))

  (set! wt-tree/lookup
        (lambda (tree key default)
          (guarantee-tree tree 'wt-tree/lookup)
          ((tree-type/lookup (tree/type tree)) tree key default)))

  (set! wt-tree/split<
        (lambda (tree key)
          (guarantee-tree tree 'wt-tree/split<)
          ((tree-type/split-lt (tree/type tree)) tree key)))

  (set! wt-tree/split>
        (lambda (tree key)
          (guarantee-tree tree 'wt-tree/split>)
          ((tree-type/split-gt (tree/type tree)) tree key)))

  (set! wt-tree/union
        (lambda (tree1 tree2)
          (guarantee-compatible-trees tree1 tree2 'wt-tree/union)
          ((tree-type/union (tree/type tree1)) tree1 tree2)))

  (set! wt-tree/intersection
        (lambda (tree1 tree2)
          (guarantee-compatible-trees tree1 tree2 'wt-tree/intersection)
          ((tree-type/intersection (tree/type tree1)) tree1 tree2)))

  (set! wt-tree/difference
        (lambda (tree1 tree2)
          (guarantee-compatible-trees tree1 tree2 'wt-tree/difference)
          ((tree-type/difference (tree/type tree1)) tree1 tree2)))

  (set! wt-tree/subset?
        (lambda (tree1 tree2)
          (guarantee-compatible-trees tree1 tree2 'wt-tree/subset?)
          ((tree-type/subset? (tree/type tree1)) tree1 tree2)))

  (set! wt-tree/set-equal?
        (lambda (tree1 tree2)
          (and (wt-tree/subset? tree1 tree2)
               (wt-tree/subset? tree2 tree1))))

  (set! wt-tree/fold
        (lambda (combiner-key-datum-result init tree)
          (guarantee-tree tree 'wt-tree/fold)
          (node/inorder-fold combiner-key-datum-result
                             init
                             (tree/root tree))))

  (set! wt-tree/for-each
        (lambda (action-key-datum tree)
          (guarantee-tree tree 'wt-tree/for-each)
          (node/for-each action-key-datum (tree/root tree))))

  (set! wt-tree/index
        (lambda (tree index)
          (guarantee-tree tree 'wt-tree/index)
          (let ((node (node/index (tree/root tree) index)))
            (and node (node/k node)))))

  (set! wt-tree/index-datum
        (lambda (tree index)
          (guarantee-tree tree 'wt-tree/index-datum)
          (let ((node (node/index (tree/root tree) index)))
            (and node (node/v node)))))

  (set! wt-tree/index-pair
        (lambda (tree index)
          (guarantee-tree tree 'wt-tree/index-pair)
          (let ((node (node/index (tree/root tree) index)))
            (and node (cons (node/k node) (node/v node))))))

  (set! wt-tree/rank
        (lambda (tree key)
          (guarantee-tree tree 'wt-tree/rank)
          ((tree-type/rank (tree/type tree)) tree key)))

  (set! wt-tree/min
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/min)
          (node/k (node/min (tree/root tree)))))

  (set! wt-tree/min-datum
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/min-datum)
          (node/v (node/min (tree/root tree)))))

  (set! wt-tree/min-pair
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/min-pair)
          (let ((node (node/min (tree/root tree))))
            (cons (node/k node) (node/v node)))))

  (set! wt-tree/delete-min
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/delete-min)
          (%make-wt-tree (tree/type tree)
                         (node/delmin (tree/root tree)))))

  (set! wt-tree/delete-min!
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/delete-min!)
          (set-tree/root! tree (node/delmin (tree/root tree)))))

  ;; < is a lexpr. Many compilers can open-code < so the lambda is faster
  ;; than passing <.
  (set! number-wt-type (local:make-wt-tree-type (lambda (u v) (< u v))))
  (set! string-wt-type (local:make-wt-tree-type string<?))

  (set! wt-tree/valid?
        (lambda (tree)
          (guarantee-tree tree 'wt-tree/valid?)
          (valid? tree)))

  'done)

;;; Local Variables:
;;; eval: (put 'with-n-node 'scheme-indent-function 1)
;;; eval: (put 'with-n-node 'scheme-indent-hook 1)
;;; End:
