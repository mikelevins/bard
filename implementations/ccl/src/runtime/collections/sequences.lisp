;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       folio - the Bard runtime
;;;; Purpose:       a congenial API for pure-functional sequences
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :cl-user)

(defpackage "FOLIO.COLLECTIONS.SEQUENCES"
  (:use :cl :as :fn)
  (:nicknames "SEQ")
  (:shadow "FIND" "INTERSECTION" "LAST" "LENGTH" "POSITION" "REDUCE" "REVERSE"
           "SEQUENCE" "SORT" "UNION")
  (:export "ADD-FIRST" "ADD-LAST" "CHOOSE-ANY" "CONCAT" "CONTAINS?" "DIFFERENCE" "DROP" "DROP-WHILE"
           "ELEMENT" "EMPTY?" "EVERY?" "FILTER" "FIND" "HEAD" "IMAGE" "INTERLEAVE" "INTERPOSE"
           "INTERSECTION" "JOIN" "LAST" "LENGTH" "MAKE" "MAKE-AS"
           "PARTITION" "POSITION" "RANGE" "REDUCE" "REPEAT" "REVERSE"
           "SELECT" "SEQUENCE?" "SHUFFLE" "SLICE" "SOME?" "SORT" "SPLIT" 
           "TAIL" "TAILS" "TAKE" "TAKE-WHILE"
           "SUBSEQUENCE" "UNION" "UNIQUE" "UNZIP" "ZIP"))

(in-package :seq)

;;; =====================================================================
;;; utils
;;; =====================================================================

(defmethod classname-for-sequence ((c cl:list)) 'cl:list)
(defmethod classname-for-sequence ((c cl:vector)) 'cl:vector)
(defmethod classname-for-sequence ((c cl:string)) 'cl:string)
(defmethod classname-for-sequence ((c fset:seq)) 'fset:seq)
(defmethod classname-for-sequence ((c fset:set)) 'fset:set)

;;; =====================================================================
;;; AS methods
;;; =====================================================================

(defmethod as ((class (eql 'cl:vector)) (thing fset:seq) &key &allow-other-keys)
  (fset:convert 'cl:vector thing))

(defmethod as ((class (eql 'fset:seq)) (thing cl:vector) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'cl:list)) (thing fset:seq) &key &allow-other-keys)
  (fset:convert 'cl:list thing))

(defmethod as ((class (eql 'fset:seq)) (thing cl:list) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'cl:string)) (thing fset:seq) &key &allow-other-keys)
  (if (fset::every 'characterp thing)
      (string (as 'vector thing))
      (format nil "~S" thing)))

(defmethod as ((class (eql 'fset:seq)) (thing cl:string) &key &allow-other-keys)
  (fset:convert 'fset:seq thing))

(defmethod as ((class (eql 'fset:seq)) (thing fset:seq) &key &allow-other-keys)
  thing)

;;; =====================================================================
;;; sequence API
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; add-first
;;; ---------------------------------------------------------------------

(defmethod add-first (x (s null))
  (list x))

(defmethod add-first (x (s list))
  (cons x s))

(defmethod add-first (x (s vector))
  (let ((v (make-array (1+ (length s))
                       :element-type (array-element-type s))))
    (setf (aref v 0) x)
    (loop for i from 1 upto (length s)
       do (setf (aref v i)(elt s (1- i))))
    v))

(defmethod add-first ((ch character) (s string))
  (let ((str (make-string (1+ (length s))
                          :element-type (array-element-type s))))
    (setf (aref str 0) ch)
    (loop for i from 1 upto (length s)
       do (setf (aref str i)(elt s (1- i))))
    str))

(defmethod add-first (x (s fset:seq))
  (fset:with s -1 x))

;;; ---------------------------------------------------------------------
;;; add-last
;;; ---------------------------------------------------------------------

(defmethod add-last ((s null) x)
  (list x))

(defmethod add-last ((s list) x)
  (append s (list x)))

(defmethod add-last ((s vector) x)
  (let ((v (make-array (1+ (length s))
                       :element-type (array-element-type s))))
    (loop for i from 0 upto (1- (length s))
       do (setf (aref v i)(elt s i)))
    (setf (aref v (length s)) x)
    v))

(defmethod add-last ((s string) (ch character))
  (let ((str (make-string (1+ (length s))
                          :element-type (array-element-type s))))
    (loop for i from 0 upto (1- (length s))
       do (setf (aref str i)(elt s i)))
    (setf (aref str (length s)) ch)
    str))

(defmethod add-last ((s fset:seq) x)
  (fset:with s (fset:size s) x))

;;; ---------------------------------------------------------------------
;;; choose-any
;;; ---------------------------------------------------------------------

(defun choose-any (s)
  (seq:element s (random (seq:length s))))

;;; ---------------------------------------------------------------------
;;; concat
;;; ---------------------------------------------------------------------

(defmethod concat (s0 s1)
  (concatenate (classname-for-sequence s0)
               s0 (as (classname-for-sequence s0) s1)))

(defmethod concat ((s0 fset:seq) s1)
  (fset:concat s0 (as 'fset:seq s1)))

;;; ---------------------------------------------------------------------
;;; contains?
;;; ---------------------------------------------------------------------

(defmethod contains? ((seq cl:sequence) x &key (test 'eql) &allow-other-keys) 
  (some (fn (v) ($ test v x)) seq))

(defmethod contains? ((seq fset:seq) x &key (test 'eql) &allow-other-keys)
  (fset:find x seq :test test))

;;; ---------------------------------------------------------------------
;;; difference
;;; ---------------------------------------------------------------------

(defmethod difference (s0 s1 &key (test 'eql))
  (as (classname-for-sequence s0)
      (let ((result nil))
        (seq:image (lambda (e) (unless (cl:find e s1 :test test)
                                 (setf result (cons e result)))) 
                   s0)
        (reverse result))))

;;; ---------------------------------------------------------------------
;;; drop
;;; ---------------------------------------------------------------------

(defmethod drop (n s &key from-end?)
  (if from-end?
      (fset:subseq s 0 (- (cl:length s) 3))
      (fset:subseq s n)))

;;; ---------------------------------------------------------------------
;;; drop-while
;;; ---------------------------------------------------------------------

(defmethod drop-while (pred s &key from-end?)
  (let ((index (fset:position-if-not pred s :from-end from-end?)))
    (let ((start (if from-end? 0 index))
          (end (if from-end? (1+ index) (fset:size s))))
      (fset:subseq s start end))))

;;; ---------------------------------------------------------------------
;;; element
;;; ---------------------------------------------------------------------

(defmethod element (s n) (fset:@ s n))

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(defmethod empty? (s) (fset:empty? s))

;;; ---------------------------------------------------------------------
;;; every?
;;; ---------------------------------------------------------------------

(defmethod every? (pred s &rest more-sequences)
  (apply 'fset::every `(,pred ,s ,@more-sequences)))

;;; ---------------------------------------------------------------------
;;; filter
;;; ---------------------------------------------------------------------

(defmethod filter (pred s)
  (as (classname-for-sequence s)
      (fset:filter pred (as 'fset:seq s))))

(defmethod filter (pred (s fset:seq))
  (fset:filter pred s))

;;; ---------------------------------------------------------------------
;;; find
;;; ---------------------------------------------------------------------

(defmethod find (pred s &key from-end?)
  (fset:find-if pred s :from-end from-end?))

;;; ---------------------------------------------------------------------
;;; head
;;; ---------------------------------------------------------------------

(defmethod head (s)(fset:@ s 0))

;;; ---------------------------------------------------------------------
;;; image
;;; ---------------------------------------------------------------------

(defun image (fn s &rest more-sequences)
  (let ((ss (cons (as 'cl:list s) 
                  (mapcar (lambda (m) (as 'cl:list m))
                          more-sequences)))) 
    (as (classname-for-sequence s)
        (apply 'mapcar fn ss))))

;;; ---------------------------------------------------------------------
;;; interleave
;;; ---------------------------------------------------------------------

(defun interleave (s0 s1)
  (as (classname-for-sequence s0)
      (apply 'append
             (as 'cl:list
                 (image (lambda (u v) (list u v))
                        s0 s1)))))

;;; ---------------------------------------------------------------------
;;; interpose
;;; ---------------------------------------------------------------------

(defmethod interpose (elem s)
  (as (classname-for-sequence s) 
      (drop 1 (interleave (repeat (length s) elem) s))))

;;; ---------------------------------------------------------------------
;;; intersection
;;; ---------------------------------------------------------------------

(defun intersection (s0 s1 &key (test 'eql))
  (as (classname-for-sequence s0)
      (cl:intersection (as 'list s0)
                       (as 'list s1)
                       :test test)))

;;; ---------------------------------------------------------------------
;;; join
;;; ---------------------------------------------------------------------

(defun join (&rest seqs)
  (if (seq:empty? seqs)
      nil
      (let* ((classname (if (stringp (seq:head seqs))
                            'list
                            (classname-for-sequence (seq:head seqs))))
             (result (as classname (seq:reduce 'seq:concat seqs :initial-value (as classname nil)))))
        (if (and (stringp (seq:head seqs))
                 (every? 'characterp result))
            (as 'string result)
            result))))

;;; ---------------------------------------------------------------------
;;; last
;;; ---------------------------------------------------------------------

(defmethod last ((s null))
  nil)

(defmethod last ((s list))
  (cl:first (cl:last s)))

(defmethod last ((s vector))
  (aref s (1- (length s))))

(defmethod last ((s string))
  (aref s (1- (length s))))

(defmethod last ((s fset:seq))
  (fset:@ s (1- (fset:size s))))

;;; ---------------------------------------------------------------------
;;; length
;;; ---------------------------------------------------------------------

(defmethod length (s)(fset:size s))

;;; ---------------------------------------------------------------------
;;; make
;;; ---------------------------------------------------------------------

(defun make (&rest args)(fset:convert 'fset:seq args))

(defun make-as (type &rest args)
  (case type
    ((cl:list) args)
    ((cl:vector) (as 'cl:vector args))
    ((cl:string) (as 'cl:string args))
    ((fset:seq) (as 'fset:seq args))
    (t (error "Unrecognized sequence type: ~S" type))))

;;; ---------------------------------------------------------------------
;;; partition
;;; ---------------------------------------------------------------------

(defmethod partition (n s &key step)
  (let ((result-class (classname-for-sequence s))
        (step (or step n)))
    (if (empty? s)
        (as result-class s)
        (if (<= (length s) n)
            (as result-class (list s))
            (as result-class
                (concat (as result-class (list (take n s)))
                        (partition n (drop step s) :step step)))))))

;;; ---------------------------------------------------------------------
;;; position
;;; ---------------------------------------------------------------------

(defmethod position (pred s)(fset:position-if pred s))

;;; ---------------------------------------------------------------------
;;; range
;;; ---------------------------------------------------------------------

(defmethod range (start end &key (by 1))
  (loop for i from start to (- end by) by by collect i))

;;; ---------------------------------------------------------------------
;;; reduce
;;; ---------------------------------------------------------------------

(defmethod reduce (fn s &key initial-value)
  (fset:reduce fn s :initial-value initial-value))

;;; ---------------------------------------------------------------------
;;; repeat
;;; ---------------------------------------------------------------------

(defmethod repeat (n item)
  (make-array n :initial-element item))

;;; ---------------------------------------------------------------------
;;; reverse
;;; ---------------------------------------------------------------------

(defmethod reverse (s)(fset:reverse s))

;;; ---------------------------------------------------------------------
;;; select
;;; ---------------------------------------------------------------------

(defmethod select (s (indexes list)) 
  (as (classname-for-sequence s)
      (seq:image 'cdr 
                 (filter (lambda (e) (find (lambda (i)(= i (car e))) indexes))
                         (zip (range 0 (length s))
                              s)))))

;;; ---------------------------------------------------------------------
;;; sequence?
;;; ---------------------------------------------------------------------

(defmethod sequence? (x) nil)
(defmethod sequence? ((s list)) t)
(defmethod sequence? ((s vector)) t)
(defmethod sequence? ((s string)) t)
(defmethod sequence? ((s fset:seq)) t)

;;; ---------------------------------------------------------------------
;;; shuffle
;;; ---------------------------------------------------------------------

(defun shuffle (s)
  (seq:sort (fn (x y)(choose-any (list t nil)))
        s))

;;; ---------------------------------------------------------------------
;;; slice
;;; ---------------------------------------------------------------------

(defmethod slice (s start &optional end)
  (fset:subseq s start end))

;;; ---------------------------------------------------------------------
;;; some?
;;; ---------------------------------------------------------------------

(defmethod some? (pred s &rest more-sequences)
  (apply 'fset::some `(,pred ,s ,@more-sequences)))

;;; ---------------------------------------------------------------------
;;; sort
;;; ---------------------------------------------------------------------

(defmethod sort (pred s)(fset:stable-sort s pred))

;;; ---------------------------------------------------------------------
;;; split
;;; ---------------------------------------------------------------------

(defun split (s sub &key (test 'eql))
  (let* ((slen (seq:length s))
         (sublen (seq:length sub))
         (indexes (seq:range 0 sublen))
         (reversed-starts nil)
         (reversed-ends nil))
    (loop for i from 0 upto (1- slen)
       do (when (seq:every? (fn (j) ($ test 
                                       (seq:element s (+ i j))
                                       (seq:element sub j))) 
                            indexes)
            (push i reversed-starts)
            (push (+ i sublen) reversed-ends)))
    (let ((result (filter (complement #'seq:empty?) 
                          (let ((starts (seq:add-first 0 (seq:reverse reversed-ends)))
                                (ends (seq:add-last (seq:reverse reversed-starts) slen)))
                            (seq:image (fn (start end)(seq:slice s start end))
                                       starts ends)))))
      (if (stringp s)
          result
          (as (classname-for-sequence s) result)))))


;;; ---------------------------------------------------------------------
;;; subsequence
;;; ---------------------------------------------------------------------

(defmethod subsequence (s start &optional end) (subseq s start end))

(defmethod subsequence ((s fset:seq) start &optional end) (fset:subseq s start end))


;;; ---------------------------------------------------------------------
;;; tail
;;; ---------------------------------------------------------------------

(defmethod tail (s) (subseq s 1))

(defmethod tail ((s cl:list)) 
  (cdr s))

(defmethod tail ((s fset:seq)) (fset:less-first s))

;;; ---------------------------------------------------------------------
;;; tails
;;; ---------------------------------------------------------------------

(defmethod tails ((s list)) 
  (if (null s)
      (list nil)
      (if (null (seq:tail s))
          (list s nil)
          (cons s (tails (cdr s))))))

(defmethod tails ((s vector)) 
  (if (seq:empty? s)
      (vector #())
      (if (seq:empty? (seq:tail s))
          (vector s #())
          (add-first s (tails (seq:tail s))))))

(defmethod tails ((s string)) 
  (if (seq:empty? s)
      (list "")
      (if (seq:empty? (seq:tail s))
          (list s "")
          (add-first s (tails (seq:tail s))))))

(defmethod tails ((s fset:seq)) 
  (if (seq:empty? s)
      (fset:seq (fset:empty-seq))
      (if (seq:empty? (seq:tail s))
          (fset:seq s (fset:empty-seq))
          (add-first s (tails (seq:tail s))))))

;;; ---------------------------------------------------------------------
;;; take
;;; ---------------------------------------------------------------------

(defmethod take (n s &key from-end?)
  (if from-end?
      (fset:subseq s n (- (cl:length s) 3))
      (fset:subseq s 0 n)))

;;; ---------------------------------------------------------------------
;;; take-while
;;; ---------------------------------------------------------------------

(defmethod take-while (pred s &key from-end?)
  (let ((index (fset:position-if-not pred s :from-end from-end?)))
    (let ((start (if from-end? (1+ index) 0))
          (end (if from-end? (fset:size s) index)))
      (fset:subseq s start end))))

;;; ---------------------------------------------------------------------
;;; union
;;; ---------------------------------------------------------------------

(defmethod union (s0 s1 &key (test 'eql))
  (unique (concat s0 s1) :test test))

;;; ---------------------------------------------------------------------
;;; unique
;;; ---------------------------------------------------------------------

(defun unique (s &key (test 'eql))
  (as (classname-for-sequence s)
      (remove-duplicates (as 'list s) :test test)))

;;; ---------------------------------------------------------------------
;;; unzip
;;; ---------------------------------------------------------------------

(defmethod unzip (s)
  (let ((sprime (as 'cl:list s)))
    (values (as (classname-for-sequence s)(mapcar 'head sprime))
            (as (classname-for-sequence s)(mapcar 'tail sprime)))))

;;; ---------------------------------------------------------------------
;;; zip
;;; ---------------------------------------------------------------------

(defmethod zip (s0 s1)
  (image (lambda (u v)
           (cons u v))
         s0 s1))
