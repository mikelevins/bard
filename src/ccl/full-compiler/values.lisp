;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.lisp
;;;; Project:       Bard - a modern Lisp
;;;; Purpose:       definitions of bard base value types
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ============================================================
;;; BARD values
;;; ============================================================

;;; ============================================================
;;; base Bard types
;;; ============================================================

(defmethod = (x y)
  (cl:= x y))

(defmethod self-evaluating? (x)
  (declare (ignore x))
  t)


;;; ------------------------------------------------------------
;;; Nothing
;;; ------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))

(defun nothing ()(make-instance 'nothing))

(defmethod nothing? (x)(declare (ignore x)) nil)
(defmethod nothing? ((x nothing))(declare (ignore x)) t)
(defun something? (x)(not (nothing? x)))

(defmethod = ((x nothing) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y nothing))
  (declare (ignore x))
  nil)

(defmethod = ((x nothing) (y nothing))
  (declare (ignore x y))
  t)

(defmethod fset:compare ((a nothing) (b nothing))
  ':equal)

(fset:define-cross-type-compare-methods nothing)

;;; ------------------------------------------------------------
;;; Number
;;; ------------------------------------------------------------

(defclass number ()
  ((value :reader value :initarg :value :type 'cl:number)))

(defmethod print-object ((n number)(str stream))
  (format str "~A" (value n)))

(defmethod initialize-instance :before ((n number) &key (value 0) &allow-other-keys)
  (assert (cl:numberp value)()
          "Invalid value for number: ~S" value))

(defmethod number ((n cl:number))
  (make-instance 'number :value n))

(defmethod number? (x)(declare (ignore x)) nil)
(defmethod number? ((x number))(declare (ignore x)) t)

(defmethod = ((x number) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y number))
  (declare (ignore x))
  nil)

(defmethod = ((x number) (y number))
  (cl:= (value x)(value y)))

(defmethod fset:compare ((a number) (b number))
  (fset:compare (value a)(value b)))

(fset:define-cross-type-compare-methods number)


;;; ------------------------------------------------------------
;;; Character
;;; ------------------------------------------------------------

(defmethod character ((c cl:character)) c)

(defmethod character? (x)(declare (ignore x)) nil)
(defmethod character? ((x cl:character))(declare (ignore x)) t)

(defmethod = ((x cl:character) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:character))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:character) (y cl:character))
  (cl:char= x y))

;;; Keyword
;;; ------------------------------------------------------------

(defclass keyword ()((name :reader name :initarg :name)))

(defmethod initialize-instance :before ((k keyword) &key (name nil) &allow-other-keys)
  (assert (cl:symbolp name)()
          "Invalid value for keyword: ~S" name))

(defmethod keyword ((s cl:symbol))
  (make-instance 'keyword :name (cl:intern (symbol-name s) (find-package :bard))))

(defmethod keyword ((s cl:string))
  (make-instance 'keyword :name (cl:intern s (find-package :bard))))

(defmethod keyword? (x)(declare (ignore x)) nil)
(defmethod keyword? ((x keyword))(declare (ignore x)) t)

(defmethod = ((x keyword) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y keyword))
  (declare (ignore x))
  nil)

(defmethod = ((x keyword) (y keyword))
  (cl:eql (name x)(name y)))

(defmethod fset:compare ((a keyword) (b keyword))
  (fset::compare-lexicographically (cl:symbol-name (name a))
                                   (cl:symbol-name (name b))))

(fset:define-cross-type-compare-methods keyword)


;;; Symbol
;;; ------------------------------------------------------------

(defclass symbol ()((name :reader name :initarg :name)))

(defmethod print-object ((s symbol)(str stream))
  (format str "~A" (name s)))

(defmethod self-evaluating? ((x symbol))
  (declare (ignore x))
  nil)

(defmethod initialize-instance :before ((s symbol) &key (name nil) &allow-other-keys)
  (assert (cl:symbolp name)()
          "Invalid value for symbol: ~S" name))

(defmethod symbol ((s cl:symbol))
  (make-instance 'symbol :name (cl:intern (symbol-name s) (find-package :bard))))

(defmethod symbol ((s cl:string))
  (make-instance 'symbol :name (cl:intern s (find-package :bard))))

(defmethod symbol? (x)(declare (ignore x)) nil)
(defmethod symbol? ((x symbol))(declare (ignore x)) t)

(defmethod = ((x symbol) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y symbol))
  (declare (ignore x))
  nil)

(defmethod = ((x symbol) (y symbol))
  (cl:eql (name x)(name y)))

(defmethod fset:compare ((a symbol) (b symbol))
  (fset::compare-lexicographically (cl:symbol-name (name a))
                                   (cl:symbol-name (name b))))

(fset:define-cross-type-compare-methods symbol)

;;; ------------------------------------------------------------
;;; Booleans
;;; ------------------------------------------------------------

(defclass boolean ()())
(defmethod boolean? (x)(declare (ignore x)) nil)
(defmethod boolean? ((x boolean))(declare (ignore x)) t)

;;; True
;;; ------------------------------------------------------------

(defclass true (boolean)()(:metaclass singleton-class))

(defun true ()(make-instance 'true))

(defmethod = ((x true) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y true))
  (declare (ignore x))
  nil)

(defmethod = ((x true) (y true))
  (declare (ignore x y))
  t)

(defmethod fset:compare ((a true) (b true))
  ':equal)

(fset:define-cross-type-compare-methods true)

;;; False
;;; ------------------------------------------------------------

(defclass false (boolean)()(:metaclass singleton-class))

(defun false ()(make-instance 'false))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)
(defmethod true? (x)(not (false? x)))

(defmethod = ((x false) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y false))
  (declare (ignore x))
  nil)

(defmethod = ((x false) (y false))
  (declare (ignore x y))
  t)

(defmethod fset:compare ((a false) (b false))
  ':equal)

(fset:define-cross-type-compare-methods false)

;;; ------------------------------------------------------------
;;; Pairs
;;; ------------------------------------------------------------

(defmethod pair? (x)(declare (ignore x)) nil)
(defmethod pair? ((x cons))(declare (ignore x)) t)

(defun pair (l r)
  (cons l r))

(defmethod left ((x cons))
  (car x))

(defmethod right ((x cons))
  (cdr x))

(defmethod = ((x cons) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cons))
  (declare (ignore x))
  nil)

(defmethod = ((x cons) (y cons))
  (and (= (left x)(left y))
       (= (right x)(right y))))

(defmethod fset:compare ((a cons) (b cons))
  (ecase (fset:compare (left a)(left b))
    ((:less) :less)
    ((:greater) :greater)
    ((:equal :unequal) (fset:compare (right a)(right b)))))

(fset:define-cross-type-compare-methods cons)


;;; ------------------------------------------------------------
;;; Sequences
;;; ------------------------------------------------------------

(defmethod sequence? (x)(declare (ignore x)) nil)
(defmethod sequence? ((x fset:seq))(declare (ignore x)) t)

(defmethod self-evaluating? ((x fset:seq))
  (declare (ignore x))
  nil)

(defmethod prepend (item (seq fset:seq))
  (fset:with-first seq item))

(defun sequence (&rest items)
  (if (null items)
    (fset:empty-seq)
    (prepend (cl:car items)
             (cl:apply 'sequence (cl:cdr items)))))

(defmethod = ((x fset:seq) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y fset:seq))
  (declare (ignore x))
  nil)

(defmethod = ((x fset:seq) (y fset:seq))
  (fset::every (lambda (i j) (= i j)) 
               x y))

;;; ------------------------------------------------------------
;;; Text
;;; ------------------------------------------------------------

(defmethod text? (x)(declare (ignore x)) nil)
(defmethod text? ((x fset:seq))
  (fset::every (lambda (c) (cl:characterp c)) x))

(defmethod text ((s string)) 
  (cl:apply 'sequence (coerce s 'list)))


;;; ------------------------------------------------------------
;;; Maps
;;; ------------------------------------------------------------

(defmethod map? (x)(declare (ignore x)) nil)
(defmethod map? ((x fset:map))(declare (ignore x)) t)

(defun map (&rest entries)
  (if (null entries)
    (fset:empty-map)
    (fset:map-union (fset:with (fset:empty-map)
                          (cl:first entries)(cl:second entries))
               (cl:apply 'map (nthcdr 2 entries)))))

(defmethod get-key ((m fset:map) key &optional (default (nothing)))
  (fset:lookup (fset:with-default m default) key))

(defmethod = ((x fset:map) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y fset:map))
  (declare (ignore x))
  nil)

(defmethod = ((x fset:map) (y fset:map))
  (and (eql (fset:compare x y)
            :equal)
       t))


#| Testing

(bard:nothing)
(bard:= (bard:nothing)(bard:nothing))
(bard:= cl:nil (bard:nothing))

(bard:number 1)
(bard:number 23.45)
(bard:number? (bard:number 23.45))
(bard:= (bard:number 1)(bard:number 1))
(bard:= (bard:number 1)(bard:number 1.0))
(bard:= (bard:number 1)(bard:number 1.1))

(bard:character #\C)
(bard:character? (bard:character #\C))
(bard:= (bard:character #\c)(bard:character #\c))

(bard:keyword "Foo")
(bard:keyword? (bard:keyword "Foo"))
(bard:= (bard:keyword '|Foo|)(bard:keyword "Foo"))

(bard:symbol "Foo")
(bard:symbol? (bard:symbol "Foo"))
(bard:= (bard:symbol '|Foo|)(bard:symbol "Foo"))

(bard:boolean? (bard:true))
(bard:boolean? (bard:false))
(bard:boolean? cl:nil)
(bard:boolean? 0)
(bard:= (bard:true)(bard:true))
(bard:= (bard:false)(bard:true))
(bard:true? (bard:true))
(bard:false? (bard:true))
(bard:false? (bard:false))

(bard:pair (bard:number 0) (bard:number 1))
(bard:left (bard:pair (bard:number 0) (bard:number 1)))
(bard:right (bard:pair (bard:number 0) (bard:number 1)))
(bard:= (bard:pair (bard:number 0) (bard:number 1))
        (bard:pair (bard:number 0) (bard:number 1)))

(bard:sequence 0 1 2 3 4 5)
(bard:prepend -1 (bard:sequence 0 1 2 3 4 5))
(bard:= (bard:sequence 0 1 2 3 4 5)
        (bard:prepend 0 (bard:sequence 1 2 3 4 5)))

(bard:text "")
(bard:text? (bard:text ""))
(bard:text "foo bar baz")
(bard:text? (bard:text "foo bar baz"))
(bard:= (bard:text "foo bar baz")
        (bard:text "foo bar baz"))


(bard:map)
(bard:map (bard:text "name") (bard:text "Fred"))
(bard:get-key (bard:map (bard:text "name") (bard:text "Barney")
                        (bard:text "age")(bard:number 45))
              (bard:text "age"))
(bard:map (bard:keyword "name") (bard:text "Fred")
          (bard:keyword "age") (bard:number 101))
(bard:get-key (bard:map (bard:text "name") (bard:text "Fred")
                        (bard:keyword "age") (bard:number 101))
              (bard:keyword "age"))

(bard:= (bard:map "name" "Fred")
        (bard:map "name" "Fred"))

|#