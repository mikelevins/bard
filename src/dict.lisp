;;;; dict.lisp
;;;; literal syntax for dictionary structures

(IN-PACKAGE :BARD.INTERNAL)
(IN-READTABLE :MODERN)

;;;------------------------------------------------------------------------------------------
;;; class dict
;;;------------------------------------------------------------------------------------------
;;; an immutable dict class that stores entries in an alist

(DEFCLASS dict ()
  ((key-test :ACCESSOR key-test :INITFORM 'EQUAL :INITARG :key-test)
   (entries :ACCESSOR entries :INITFORM NIL :INITARG :entries)))

(DEFMETHOD PRINT-OBJECT ((dict dict) stream)
  (LET* ((entries (entries dict))
         (first-entry (FIRST entries))
         (rest-entries (REST entries)))
    (FORMAT stream "{")
    (WHEN first-entry
      (FORMAT stream "~s ~s" (CAR first-entry)(CDR first-entry)))
    (LOOP FOR entry IN rest-entries
       DO (LET ((*PRINT-PRETTY* NIL))
            (FORMAT stream " ~s ~s" (CAR entry)(CDR entry))))
    (FORMAT stream "}")))

;;; common dict protocol

(DEFMETHOD all-keys ((dict dict) &KEY &ALLOW-OTHER-KEYS)
  (MAPCAR 'CAR (entries dict)))

(DEFMETHOD all-values ((dict dict) &KEY &ALLOW-OTHER-KEYS)
  (MAPCAR 'CDR (entries dict)))


(DEFMETHOD contains-key? ((dict dict) key)
  (IF (ASSOC key (entries dict) :TEST (key-test dict))
      T
      NIL))


(DEFMETHOD contains-value? ((dict dict) value &KEY (test 'EQUAL))
  (AND (FIND-IF (^ (e)(call test value (CDR e)))
                (entries dict))
       T))


(DEFMETHOD copy-dict ((dict dict))
  (MAKE-INSTANCE 'dict
                 :key-test (key-test dict)
                 :entries (COPY-TREE (entries dict))))


(DEFUN dict (key-test &REST contents)
  (LET ((entries (LOOP FOR tail ON contents BY #'CDDR
                    COLLECT (CONS (FIRST tail)(SECOND tail)))))
    (MAKE-INSTANCE 'dict :key-test key-test :entries entries)))

;;; !!! Case conversion needed below
#|

(defmethod dict? (thing) nil)
(defmethod dict? ((thing dict)) t)

(defmethod empty? ((dict dict) &key &allow-other-keys)
  (null (entries dict)))

(defmethod get-key ((dict dict) key &key (default nil))
  (let ((entry (assoc key (entries dict) :test (key-test dict))))
    (if entry
        (cdr entry)
        default)))

(defmethod merge-dicts ((left dict) (right dict))
  (let* ((key-test (key-test left))
         (left-entries (entries left))
         (right-entries (entries right))
         (new-entries (copy-tree left-entries)))
    (loop for e in right-entries
       do (let ((already-entry (assoc (car e) new-entries :test key-test)))
            (if already-entry
                (setf (cdr already-entry)
                      (cdr e))
                (setf new-entries
                      (cons (cons (car e)
                                  (cdr e))
                            new-entries)))))
    (make-instance 'dict :key-test key-test :entries new-entries)))

(defmethod put-key ((dict dict) key value &key (test 'equal) (default nil))
  (let* ((already-entry (assoc key (entries dict) :test (key-test dict)))
         (new-entry (cons key value))
         (new-entries (if already-entry
                          (remove key (entries dict) :test (key-test dict) :key 'car)
                          (entries dict))))
    (make-instance 'dict
          :key-test (key-test dict)
          :entries (cons new-entry new-entries))))

(defmethod remove-key ((dict dict) key &key test &allow-other-keys)
  (let* ((found-entry (assoc key (entries dict) :test (key-test dict)))
         (new-entries (if found-entry
                          (remove key (entries dict) :test (key-test dict) :key 'car)
                          (entries dict))))
    (make-instance 'dict
          :key-test (key-test dict)
          :entries new-entries)))

(defmethod select-keys ((dict dict) keys &key test &allow-other-keys)
  (let ((key-test (key-test dict)))
    (make-instance 'dict
          :key-test key-test
          :entries (remove-if-not (^ (entry)(member (car entry) keys :test (or test (key-test dict))))
                                  (entries dict)))))

(defmethod select-complement-keys ((dict dict) keys &key test &allow-other-keys)
  (let ((key-test (key-test dict)))
    (make-instance 'dict
                   :key-test key-test
                   :entries (remove-if (^ (entry)(member (car entry) keys :test (or test (key-test dict))))
                                       (entries dict)))))

;;; (setf $d (dict 'equal :a 1 :b 2 :c 3 :d 4))
;;; (select-keys $d '(:a :c))
;;; (select-complement-keys $d '(:a :c))

;;; immutable dicts
(defmethod immutable-dict? (thing) nil)
(defmethod immutable-dict? ((thing dict)) t) 

;;;------------------------------------------------------------------------------------------
;;; class mutable-dict
;;;------------------------------------------------------------------------------------------
;;; a mutable dict class that stores entries in an alist

(defclass mutable-dict (dict)())

;;; mutable dicts

(defun mutable-dict (key-test &rest contents)
  (let ((entries (loop for tail on contents by #'cddr
                    collect (cons (first tail)(second tail)))))
    (make-instance 'mutable-dict :key-test key-test :entries entries)))

(defmethod mutable-dict? (thing) nil)
(defmethod mutable-dict? ((dict mutable-dict)) t)

(defmethod merge-into! ((left mutable-dict) (right dict))
  (let ((key-test (key-test left)))
    (loop for entry in (entries right)
       do (let ((found-entry (assoc (car entry) (entries left) :test key-test)))
            (if found-entry
                (setf (cdr found-entry) (cdr entry))
                (setf (entries left)
                      (cons (cons (car entry)(cdr entry))
                            (entries left))))))
    left))

(defmethod remove-key! ((dict mutable-dict) key &key test &allow-other-keys)
  (let ((found-entry (assoc key (entries dict) :test (key-test dict))))
    (when found-entry
      (let ((new-entries (remove key (entries dict) :test (key-test dict) :key 'car)))
        (setf (entries dict) new-entries)))
    dict))

(defmethod set-key! ((dict mutable-dict) key value &key test &allow-other-keys)
  (let ((already-entry (assoc key (entries dict) :test (or test (key-test dict)))))
    (if already-entry
        (setf (cdr already-entry) value)
        (setf (entries dict)
              (cons (cons key value)
                    (entries dict))))
    dict))

|#


;;; (setf $dict1 (dict 'equal "name" "fred" "age" 35))
;;; (all-keys $dict1)
;;; (all-values $dict1)
;;; (contains-key? $dict1 "name")
;;; (contains-value? $dict1 "fred")
;;; (setf $dict2 (copy-dict $dict1))
;;; (get-key $dict2 "name")
;;; (get-key $dict2 "shape" :default :none)
;;; (setf $dict3 (merge-dicts $dict1 (dict 'equal "name" "barney")))
;;; (setf $dict4 (put-key $dict1 "color" "orange"))
;;; (remove-key $dict4 "age")
;;; (select-keys $dict4 (list "age" "color" "shape"))
;;; (setf $dict5 (mutable-dict 'equal "name" "fred" "age" 35))
;;; (all-keys $dict5)
;;; (merge-into! $dict5 (dict 'equal "name" "barney" "size" "small"))
;;; (set-key! $dict5 "size" "little")
;;; (set-key! $dict5 "color" "brown")
;;; (remove-key! $dict5 "size")
