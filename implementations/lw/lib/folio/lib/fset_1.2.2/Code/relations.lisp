;;; -*- Mode: Lisp; Package: FSet; Syntax: ANSI-Common-Lisp -*-

(in-package :fset)

;;; File: relations.lisp
;;; Contents: Relations (binary and general).
;;;
;;; This file is part of FSet.  Copyright (c) 2007 Sympoiesis, Inc.
;;; FSet is licensed under the Lisp Lesser GNU Public License, or LLGPL.
;;; See: http://opensource.franz.com/preamble.html
;;; This license provides NO WARRANTY.


(defstruct (relation
	    (:include collection)
	    (:constructor nil)
	    (:predicate relation?)
	    (:copier nil))
  "The abstract class for FSet relations.  It is a structure class.")

(defgeneric arity (rel)
  (:documentation "Returns the arity of the relation `rel'."))

(defstruct (2-relation
	    (:include relation)
	    (:constructor nil)
	    (:predicate 2-relation?)
	    (:copier nil))
  "The abstract class for FSet binary relations.  It is a structure class.")

(defmethod arity ((br 2-relation))
  2)

(defstruct (wb-2-relation
	    (:include 2-relation)
	    (:constructor make-wb-2-relation (size map0 map1))
	    (:predicate wb-2-relation?)
	    (:print-function print-wb-2-relation)
	    (:copier nil))
  "A class of functional binary relations represented as pairs of weight-
balanced binary trees.  This is the default implementation of binary relations
in FSet.  The inverse is constructed lazily, and maintained thereafter."
  size
  map0
  map1)

(defparameter *empty-wb-2-relation* (make-wb-2-relation 0 nil nil))

(defun empty-2-relation ()
  *empty-wb-2-relation*)
(declaim (inline empty-2-relation))

(defun empty-wb-2-relation ()
  *empty-wb-2-relation*)
(declaim (inline empty-wb-2-relation))

(defmethod empty? ((br wb-2-relation))
  (zerop (wb-2-relation-size br)))

(defmethod size ((br wb-2-relation))
  (wb-2-relation-size br))

(defmethod arb ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Arb-Pair tree)))
	  (values key (WB-Set-Tree-Arb val)) t)
      (values nil nil nil))))

;;; Must pass the pair as a cons -- the generic function doesn't allow us to
;;; add a parameter.  (&&& Actually we should do the same thing we're doing
;;; with `with' and `less'.)
(defmethod contains? ((br wb-2-relation) pr)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) (car pr))))
    (and found? (WB-Set-Tree-Member? set-tree (cdr pr)))))

;;; Returns the range set.
(defmethod lookup ((br wb-2-relation) x)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x)))
    (if found? (make-wb-set set-tree)
      *empty-wb-set*)))

(defgeneric lookup-inv (2-relation y)
  (:documentation "Does an inverse lookup on a binary relation."))

(defmethod lookup-inv ((br wb-2-relation) y)
  (get-inverse br)
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map1 br) y)))
    (if found? (make-wb-set set-tree)
      *empty-wb-set*)))

(defmethod domain ((br wb-2-relation))
  (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map0 br))))

(defmethod range ((br wb-2-relation))
  (get-inverse br)
  (make-wb-set (WB-Map-Tree-Domain (wb-2-relation-map1 br))))

(defun get-inverse (br)
  (let ((m0 (wb-2-relation-map0 br))
	(m1 (wb-2-relation-map1 br)))
    (when (and m0 (null m1))
      (Do-WB-Map-Tree-Pairs (x s m0)
	(Do-WB-Set-Tree-Members (y s)
	  (let ((ignore prev (WB-Map-Tree-Lookup m1 y)))
	    (declare (ignore ignore))
	    (setq m1 (WB-Map-Tree-With m1 y (WB-Set-Tree-With prev x))))))
      ;;; Look Ma, no locking!  Assuming the write is atomic.
      (setf (wb-2-relation-map1 br) m1))
    m1))

(defgeneric inverse (2-relation)
  (:documentation "The inverse of a binary relation."))

;;; This is so fast (once the inverse is constructed) we almost don't need
;;; `lookup-inv'.  Maybe we should just put a compiler optimizer on
;;; `(lookup (inverse ...) ...)'?
(defmethod inverse ((br wb-2-relation))
  (get-inverse br)
  (make-wb-2-relation (wb-2-relation-size br) (wb-2-relation-map1 br)
		      (wb-2-relation-map0 br)))

(defmethod least ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Least-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod greatest ((br wb-2-relation))
  (let ((tree (wb-2-relation-map0 br)))
    (if tree
	(let ((key val (WB-Map-Tree-Greatest-Pair tree)))
	  (values key val t))
      (values nil nil nil))))

(defmethod with ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x))
	(map1 (wb-2-relation-map1 br)))
    (if found?
	(let ((new-set-tree (WB-Set-Tree-With set-tree y)))
	  (if (eq new-set-tree set-tree)
	      br			; `y' was already there
	    (make-wb-2-relation (1+ (wb-2-relation-size br))
			     (WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree)
			     (and map1
				  (let ((ignore set-tree-1
					  (WB-Map-Tree-Lookup map1 y)))
				    (declare (ignore ignore))
				    (WB-Map-Tree-With
				      map1 y (WB-Set-Tree-With set-tree-1 x)))))))
      (make-wb-2-relation (1+ (wb-2-relation-size br))
		       (WB-Map-Tree-With (wb-2-relation-map0 br) x
					 (WB-Set-Tree-With nil y))
		       (and map1
			    (let ((ignore set-tree-1
				    (WB-Map-Tree-Lookup map1 y)))
			      (declare (ignore ignore))
			      (WB-Map-Tree-With
				map1 y (WB-Set-Tree-With set-tree-1 x))))))))

(defmethod less ((br wb-2-relation) x &optional (y nil y?))
  ;; Try to provide a little support for the cons representation of pairs.
  (unless y?
    (setq y (cdr x) x (car x)))
  (let ((found? set-tree (WB-Map-Tree-Lookup (wb-2-relation-map0 br) x))
	(map1 (wb-2-relation-map1 br)))
    (if (not found?)
	br
      (let ((new-set-tree (WB-Set-Tree-Less set-tree y)))
	(if (eq new-set-tree set-tree)
	    br
	  (make-wb-2-relation (1- (wb-2-relation-size br))
			   (if new-set-tree
			       (WB-Map-Tree-With (wb-2-relation-map0 br) x new-set-tree)
			     (WB-Map-Tree-Less (wb-2-relation-map0 br) x))
			   (and map1
				(let ((ignore set-tree
					(WB-Map-Tree-Lookup map1 y))
				      ((new-set-tree (WB-Set-Tree-Less set-tree x))))
				  (declare (ignore ignore))
				  (if new-set-tree
				      (WB-Map-Tree-With map1 y new-set-tree)
				    (WB-Map-Tree-Less map1 y))))))))))

(defmethod union ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (let ((new-size 0)
	((new-map0 (WB-Map-Tree-Union (wb-2-relation-map0 br1) (wb-2-relation-map0 br2)
				      (lambda (ignore s1 s2)
					(declare (ignore ignore))
					(let ((s (WB-Set-Tree-Union s1 s2)))
					  (incf new-size (WB-Set-Tree-Size s))
					  s))))
	 (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			(WB-Map-Tree-Union (wb-2-relation-map1 br1)
					   (wb-2-relation-map1 br2)
					   (lambda (ignore s1 s2)
					     (declare (ignore ignore))
					     (WB-Set-Tree-Union s1 s2)))))))
    (make-wb-2-relation new-size new-map0 new-map1)))

(defmethod intersection ((br1 wb-2-relation) (br2 wb-2-relation) &key)
  (let ((new-size 0)
	((new-map0 (WB-Map-Tree-Intersect (wb-2-relation-map0 br1)
					  (wb-2-relation-map0 br2)
					  (lambda (ignore s1 s2)
					    (declare (ignore ignore))
					    (let ((s (WB-Set-Tree-Intersect s1 s2)))
					      (incf new-size (WB-Set-Tree-Size s))
					      (values s s)))))
	 (new-map1 (and (or (wb-2-relation-map1 br1) (wb-2-relation-map1 br2))
			(WB-Map-Tree-Intersect (wb-2-relation-map1 br1)
					       (wb-2-relation-map1 br2)
					       (lambda (ignore s1 s2)
						 (declare (ignore ignore))
						 (let ((s (WB-Set-Tree-Intersect s1 s2)))
						   (values s s))))))))
    (make-wb-2-relation new-size new-map0 new-map1)))

(defgeneric join (relation-a column-a relation-b column-b)
  (:documentation
    "A relational equijoin, matching up `column-a' of `relation-a' with `column-b' of
`relation-b'.  For a binary relation, the columns are named 0 (domain) and 1 (range)."))

(defmethod join ((bra wb-2-relation) cola (brb wb-2-relation) colb)
  (let ((map0a map1a (ecase cola
		       (1 (values (wb-2-relation-map0 bra) (wb-2-relation-map1 bra)))
		       (0 (progn
			    (get-inverse bra)
			    (values (wb-2-relation-map1 bra)
				    (wb-2-relation-map0 bra))))))
	(map0b map1b (ecase colb
		       (0 (values (wb-2-relation-map0 brb) (wb-2-relation-map1 brb)))
		       (1 (progn
			    (get-inverse brb)
			    (values (wb-2-relation-map1 brb)
				    (wb-2-relation-map0 brb))))))
	(new-map0 nil)
	(new-map1 nil)
	(new-size 0))
    (Do-WB-Map-Tree-Pairs (x ys map0a)
      (Do-WB-Set-Tree-Members (y ys)
	(let ((ignore s (WB-Map-Tree-Lookup map0b y)))
	  (declare (ignore ignore))
	  (when s
	    (let ((ignore prev (WB-Map-Tree-Lookup new-map0 x))
		  ((new (WB-Set-Tree-Union prev s))))
	      (declare (ignore ignore))
	      (incf new-size (- (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev)))
	      (setq new-map0 (WB-Map-Tree-With new-map0 x new)))))))
    (when (or map1a map1b)
      (when (null map1b)
	(setq map1b (get-inverse brb)))
      (when (null map1a)
	(setq map1a (get-inverse bra)))
      (Do-WB-Map-Tree-Pairs (x ys map1b)
	(Do-WB-Set-Tree-Members (y ys)
	  (let ((ignore s (WB-Map-Tree-Lookup map1a y)))
	    (declare (ignore ignore))
	    (when s
	      (let ((ignore prev (WB-Map-Tree-Lookup new-map1 x)))
		(declare (ignore ignore))
		(setq new-map1
		      (WB-Map-Tree-With new-map1 x (WB-Set-Tree-Union prev s)))))))))
    (make-wb-2-relation new-size new-map0 new-map1)))


(defgeneric internal-do-2-relation (br elt-fn value-fn))

(defmacro do-2-relation ((key val br &optional value) &body body)
  `(block nil
     (internal-do-2-relation ,br (lambda (,key ,val) . ,body)
			      (lambda () ,value))))

(defmethod internal-do-2-relation ((br wb-2-relation) elt-fn value-fn)
  (Do-WB-Map-Tree-Pairs (x y-set (wb-2-relation-map0 br) (funcall value-fn))
    (Do-WB-Set-Tree-Members (y y-set)
      (funcall elt-fn x y))))

(defmethod convert ((to-type (eql '2-relation)) (br 2-relation) &key)
  br)

(defmethod convert ((to-type (eql 'wb-2-relation)) (br wb-2-relation) &key)
  br)

(defmethod convert ((to-type (eql 'set)) (br 2-relation) &key (pair-fn #'cons))
  (let ((result nil)
	(pair-fn (coerce pair-fn 'function)))
    (do-2-relation (x y br)
      (setq result (WB-Set-Tree-With result (funcall pair-fn x y))))
    (make-wb-set result)))

(defmethod convert ((to-type (eql '2-relation)) (m map) &key from-type)
  "If `from-type' is the symbol `map-to-sets', the range elements must all be
sets, and the result pairs each domain element with each member of the
corresponding range set.  Otherwise, the result pairs each domain element
with the corresponding range element directly."
  (if (eq from-type 'map-to-sets)
      (map-to-sets-to-wb-2-relation m)
    (map-to-wb-2-relation m)))

(defmethod convert ((to-type (eql 'wb-2-relation)) (m map) &key from-type)
  "If `from-type' is the symbol `map-to-sets', the range elements must all be
sets, and the result pairs each domain element with each member of the
corresponding range set.  Otherwise, the result pairs each domain element
with the corresponding range element directly."
  (if (eq from-type 'map-to-sets)
      (map-to-sets-to-wb-2-relation m)
    (map-to-wb-2-relation m)))

(defun map-to-sets-to-wb-2-relation (m)
  (let ((size 0)
	((new-tree (WB-Map-Tree-Compose
		     (wb-map-contents m)
		     #'(lambda (s)
			 (let ((s (wb-set-contents (convert 'wb-set s))))
			   (incf size (WB-Set-Tree-Size s))
			   s))))))
    (make-wb-2-relation size new-tree nil)))

(defun map-to-wb-2-relation (m)
  (let ((new-tree (WB-Map-Tree-Compose (wb-map-contents m)
				       #'(lambda (x) (WB-Set-Tree-With nil x)))))
    (make-wb-2-relation (size m) new-tree nil)))

(defmethod convert ((to-type (eql '2-relation)) (alist list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (list-to-wb-2-relation alist key-fn value-fn))

(defmethod convert ((to-type (eql 'wb-2-relation)) (alist list)
		    &key (key-fn #'car) (value-fn #'cdr))
  (list-to-wb-2-relation alist key-fn value-fn))

(defun list-to-wb-2-relation (alist key-fn value-fn)
  (let ((m0 nil)
	(size 0)
	(key-fn (coerce key-fn 'function))
	(value-fn (coerce value-fn 'function)))
    (dolist (pr alist)
      (let ((k (funcall key-fn pr))
	    (v (funcall value-fn pr))
	    ((found? prev (WB-Map-Tree-Lookup m0 k))
	     ((new (WB-Set-Tree-With prev v)))))
	(declare (ignore found?))
	(when (> (WB-Set-Tree-Size new) (WB-Set-Tree-Size prev))
	  (incf size)
	  (setq m0 (WB-Map-Tree-With m0 k new)))))
    (make-wb-2-relation size m0 nil)))

(defmethod convert ((to-type (eql 'map)) (br wb-2-relation) &key)
  (2-relation-to-wb-map br))

(defmethod convert ((to-type (eql 'wb-map)) (br wb-2-relation) &key)
  (2-relation-to-wb-map br))

(defun 2-relation-to-wb-map (br)
  (let ((m nil))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (let ((sz (WB-Set-Tree-Size s)))
	(unless (= 1 sz)
	  (error "2-relation maps ~A to ~D values" x sz))
	(setq m (WB-Map-Tree-With m x (WB-Set-Tree-Arb s)))))
    (make-wb-map m)))

(defgeneric conflicts (2-relation)
  (:documentation
    "Returns a 2-relation containing only those pairs of `2-relation' whose domain value
is mapped to multiple range values."))

(defmethod conflicts ((br wb-2-relation))
  (let ((m0 nil)
	(size 0))
    (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br))
      (when (> (WB-Set-Tree-Size s) 1)
	(setq m0 (WB-Map-Tree-With m0 x s))
	(incf size (WB-Set-Tree-Size s))))
    (make-wb-2-relation size m0 nil)))

(defun print-wb-2-relation (br stream level)
  (if (and *print-level* (>= level *print-level*))
      (format stream "#")
    (progn
      (format stream "#{+ ")
      (let ((i 0))
	(do-2-relation (x y br)
	  (when (> i 0)
	    (format stream " "))
	  (when (and *print-length* (>= i *print-length*))
	    (format stream "...")
	    (return))
	  (incf i)
	  (let ((*print-level* (and *print-level* (1- *print-level*))))
	    (write (list x y) :stream stream)))
	(when (> i 0)
	  (format stream " ")))
      (format stream "+}"))))

(def-gmap-res-type :2-relation (&key filterp)
  "Consumes two values from the mapped function; returns a 2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (alist x y) (cons (cons x y) alist)))
	#'(lambda (alist) (list-to-wb-2-relation alist #'car #'cdr))
	,filterp))

(def-gmap-res-type :wb-2-relation (&key filterp)
  "Consumes two values from the mapped function; returns a 2-relation of the pairs.
Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (alist x y) (cons (cons x y) alist)))
	#'(lambda (alist) (list-to-wb-2-relation alist #'car #'cdr))
	,filterp))


(define-cross-type-compare-methods relation)

(defmethod compare ((a wb-2-relation) (b wb-2-relation))
  (WB-Map-Tree-Compare (wb-2-relation-map0 a) (wb-2-relation-map0 b)
		       #'WB-Set-Tree-Compare))

(defmethod verify ((br wb-2-relation))
  ;; Slow, but thorough.
  (and (WB-Map-Tree-Verify (wb-2-relation-map0 br))
       (WB-Map-Tree-Verify (wb-2-relation-map1 br))
       (let ((size 0))
	 (and (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map0 br) t)
		(WB-Set-Tree-Verify s)
		(incf size (WB-Set-Tree-Size s))
		(or (null (wb-2-relation-map1 br))
		    (Do-WB-Set-Tree-Members (y s)
		      (let ((ignore s1 (WB-Map-Tree-Lookup (wb-2-relation-map1 br) y)))
			(declare (ignore ignore))
			(unless (WB-Set-Tree-Member? s1 x)
			  (format *debug-io* "Map discrepancy in wb-2-relation")
			  (return nil))))))
	      (or (= size (wb-2-relation-size br))
		  (progn (format *debug-io* "Size discrepancy in wb-2-relation")
			 nil))))
       (or (null (wb-2-relation-map1 br))
	   (let ((size 0))
	     (Do-WB-Map-Tree-Pairs (x s (wb-2-relation-map1 br))
	       (declare (ignore x))
	       (WB-Set-Tree-Verify s)
	       (incf size (WB-Set-Tree-Size s)))
	     (or (= size (wb-2-relation-size br))
		 (progn (format *debug-io*  "Size discrepancy in wb-2-relation")
			nil))))))


(defgeneric transitive-closure (2-relation set)
  (:documentation
    "The transitive closure of the set over the relation.  The relation may
also be supplied as a function returning a set."))

(defmethod transitive-closure ((fn function) (s set))
  (set-transitive-closure fn s))

(defmethod transitive-closure ((r 2-relation) (s set))
  (set-transitive-closure r s))

(defun set-transitive-closure (r s)
  ;; This could probably use a little moer work.
  (let ((workset (set-difference
		   (reduce #'union (image r (convert 'seq s)) :initial-value (set))
		   s))
	(result s))
    (while (nonempty? workset)
      (let ((x (arb workset)))
	(removef workset x)
	(adjoinf result x)
	(unionf workset (set-difference (@ r x) result))))
    result))
