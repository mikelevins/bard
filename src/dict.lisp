;;;; dict.lisp
;;;; literal syntax for dictionary structures

(IN-PACKAGE :bard.internal)

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
      (FORMAT stream "~S ~S" (CAR first-entry)(CDR first-entry)))
    (LOOP FOR entry IN rest-entries
       DO (LET ((*PRINT-PRETTY* NIL))
            (FORMAT stream " ~S ~S" (CAR entry)(CDR entry))))
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
(DEFMETHOD dict? ((thing dict)) t)

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
  (LET ((key-test (key-test dict)))
    (MAKE-INSTANCE 'dict
          :key-test key-test
          :entries (REMOVE-IF-NOT (^ (entry)(MEMBER (CAR entry) keys :TEST (OR test (key-test dict))))
                                  (entries dict)))))

(DEFMETHOD select-complement-keys ((dict dict) keys &KEY test &ALLOW-OTHER-KEYS)
  (LET ((key-test (key-test dict)))
    (MAKE-INSTANCE 'dict
                   :key-test key-test
                   :entries (REMOVE-IF (^ (entry)(MEMBER (CAR entry) keys :TEST (OR test (key-test dict))))
                                       (entries dict)))))

;;; (SETF $d (dict 'EQUAL :a 1 :b 2 :c 3 :d 4))
;;; (select-keys $d '(:a :c))
;;; (select-complement-keys $d '(:a :c))

;;; immutable dicts
(DEFMETHOD immutable-dict? (thing) NIL)
(DEFMETHOD immutable-dict? ((thing dict)) T) 

;;;------------------------------------------------------------------------------------------
;;; class mutable-dict
;;;------------------------------------------------------------------------------------------
;;; a mutable dict class that stores entries in an alist

(DEFCLASS mutable-dict (dict)())

;;; mutable dicts

(DEFUN mutable-dict (key-test &REST contents)
  (LET ((entries (LOOP FOR tail ON contents BY #'CDDR
                    COLLECT (CONS (FIRST tail)(SECOND tail)))))
    (MAKE-INSTANCE 'mutable-dict :key-test key-test :entries entries)))

(DEFMETHOD mutable-dict? (thing) NIL)
(DEFMETHOD mutable-dict? ((dict mutable-dict)) T)

(DEFMETHOD merge-into! ((left mutable-dict) (right dict))
  (LET ((key-test (key-test left)))
    (LOOP FOR entry IN (entries right)
       DO (LET ((found-entry (ASSOC (CAR entry) (entries left) :TEST key-test)))
            (IF found-entry
                (SETF (CDR found-entry) (CDR entry))
                (SETF (entries left)
                      (CONS (CONS (CAR entry)(CDR entry))
                            (entries left))))))
    left))

(DEFMETHOD remove-key! ((dict mutable-dict) key &KEY test &ALLOW-OTHER-KEYS)
  (LET ((found-entry (ASSOC key (entries dict) :TEST (key-test dict))))
    (WHEN found-entry
      (LET ((new-entries (REMOVE key (entries dict) :TEST (key-test dict) :KEY 'CAR)))
        (SETF (entries dict) new-entries)))
    dict))

(DEFMETHOD set-key! ((dict mutable-dict) key value &KEY test &allow-other-keys)
  (LET ((already-entry (ASSOC key (entries dict) :TEST (OR test (key-test dict)))))
    (IF already-entry
        (SETF (CDR already-entry) value)
        (SETF (entries dict)
              (CONS (CONS key value)
                    (entries dict))))
    dict))

;;; (SETF $dict1 (dict 'EQUAL "name" "Fred" "age" 35))
;;; (all-keys $dict1)
;;; (all-values $dict1)
;;; (contains-key? $dict1 "names")
;;; (contains-value? $dict1 "Fred")
;;; (SETF $dict2 (copy-dict $dict1))
;;; (get-key $dict2 "name")
;;; (get-key $dict2 "shape" :default :none)
;;; (SETF $dict3 (merge-dicts $dict1 (dict 'equal "name" "Barney")))
;;; (SETF $dict4 (put-key $dict1 "color" "orange"))
;;; (remove-key $dict4 "age")
;;; (select-keys $dict4 (LIST "age" "color" "shape"))
;;; (SETF $dict5 (mutable-dict 'EQUAL "name" "Fred" "age" 35))
;;; (all-keys $dict5)
;;; (merge-into! $dict5 (dict 'EQUAL "name" "Barney" "size" "small"))
;;; (set-key! $dict5 "size" "little")
;;; (set-key! $dict5 "color" "brown")
;;; (remove-key! $dict5 "size")
