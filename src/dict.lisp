;;;; dict.lisp
;;;; literal syntax for dictionary structures

(in-package :bard.internal)

;;;------------------------------------------------------------------------------------------
;;; class dict
;;;------------------------------------------------------------------------------------------
;;; an immutable dict class that stores entries in an alist

(defclass dict ()
  ((key-test :accessor key-test :initform 'equal :initarg :key-test)
   (entries :accessor entries :initform nil :initarg :entries)))

(defmethod print-object ((dict dict) stream)
  (let* ((entries (entries dict))
         (first-entry (first entries))
         (rest-entries (rest entries)))
    (format stream "{")
    (when first-entry
      (format stream "~s ~s" (car first-entry)(cdr first-entry)))
    (loop for entry in rest-entries
       do (let ((*print-pretty* nil))
            (format stream " ~s ~s" (car entry)(cdr entry))))
    (format stream "}")))

;;; common dict protocol
(defmethod all-keys ((dict dict) &key &allow-other-keys)
  (mapcar 'car (entries dict)))

(defmethod all-values ((dict dict) &key &allow-other-keys)
  (mapcar 'cdr (entries dict)))

(defmethod contains-key? ((dict dict) key)
  (if (assoc key (entries dict) :test (key-test dict))
      t
      nil))

(defmethod contains-value? ((dict dict) value &key (test 'equal))
  (and (find-if (^ (e)(call test value (cdr e)))
                (entries dict))
       t))

(defmethod copy-dict ((dict dict))
  (make-instance 'dict
                 :key-test (key-test dict)
                 :entries (copy-tree (entries dict))))

(defun dict (key-test &rest contents)
  (let ((entries (loop for tail on contents by #'cddr
                    collect (cons (first tail)(second tail)))))
    (make-instance 'dict :key-test key-test :entries entries)))

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
