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



(DEFMETHOD dict? (thing) NIL)
(DEFMETHOD dict? ((thing dict)) T)


(DEFMETHOD empty? ((dict dict) &KEY &ALLOW-OTHER-KEYS)
  (NULL (entries dict)))

(DEFMETHOD get-key ((dict dict) key &KEY (default NIL))
  (LET ((entry (ASSOC key (entries dict) :TEST (key-test dict))))
    (IF entry
        (CDR entry)
        default)))

(DEFMETHOD merge-dicts ((left dict) (right dict))
  (LET* ((key-test (key-test left))
         (left-entries (entries left))
         (right-entries (entries right))
         (new-entries (COPY-TREE left-entries)))
    (LOOP FOR e IN right-entries
       DO (LET ((already-entry (ASSOC (CAR e) new-entries :TEST key-test)))
            (IF already-entry
                (SETF (CDR already-entry)
                      (CDR e))
                (SETF new-entries
                      (CONS (CONS (CAR e)
                                  (CDR e))
                            new-entries)))))
    (MAKE-INSTANCE 'dict :key-test key-test :entries new-entries)))

(DEFMETHOD put-key ((dict dict) key value &KEY (test 'EQUAL) (default NIL))
  (LET* ((already-entry (ASSOC key (entries dict) :TEST (key-test dict)))
         (new-entry (CONS key value))
         (new-entries (IF already-entry
                          (REMOVE key (entries dict) :TEST (key-test dict) :KEY 'CAR)
                          (entries dict))))
    (MAKE-INSTANCE 'dict
          :key-test (key-test dict)
          :entries (CONS new-entry new-entries))))

(DEFMETHOD remove-key ((dict dict) key &KEY test &ALLOW-OTHER-KEYS)
  (LET* ((found-entry (ASSOC key (entries dict) :TEST (key-test dict)))
         (new-entries (IF found-entry
                          (REMOVE key (entries dict) :TEST (key-test dict) :KEY 'CAR)
                          (entries dict))))
    (MAKE-INSTANCE 'dict
          :key-test (key-test dict)
          :entries new-entries)))


(DEFMETHOD select-keys ((dict dict) keys &KEY test &ALLOW-OTHER-KEYS)
  (LET* ((key-test (key-test dict))
         (old-entries (entries dict))
         (new-entries (REMOVE-IF-NOT (LAMBDA (entry)(MEMBER (CAR entry) keys :TEST key-test))
                                     old-entries)))
    (MAKE-INSTANCE 'dict
                   :key-test test
                   :entries new-entries)))

#+NIL (define $fred
          (MAKE-INSTANCE 'dict
                         :key-test 'EQUAL
                         :entries '((:name . "Fred")(:age . 35)(:shape . :square)
                                    (:color . :orange))))


(DEFMETHOD select-complement-keys ((dict dict) keys &KEY test &ALLOW-OTHER-KEYS)
  (LET* ((key-test (key-test dict))
         (old-entries (entries dict))
         (new-entries (REMOVE-IF (LAMBDA (entry)(MEMBER (CAR entry) keys :TEST key-test))
                                 old-entries)))
    (MAKE-INSTANCE 'dict
                   :key-test test
                   :entries new-entries)))

;;; converting case below
#|

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

;;; done converting case
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
