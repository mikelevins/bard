;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Bard.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       build the Bard application image
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "CL-USER")

;;; ========================================================================
;;; Package definitions
;;; ========================================================================

(defpackage "BARD"
  (:use "COMMON-LISP" "CCL")
  (:shadow "=" "APPEND" "BOOLEAN" "CONCATENATE" "FALSE" "MAP" "TRUE"))

(in-package "BARD")

;;; ========================================================================
;;; Values
;;; ========================================================================

;;; singleton-class serves as a metaclass for unique values

(defclass singleton-class (standard-class)
  ((instance :accessor instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)(superclass standard-class)) t)
(defmethod validate-superclass ((class singleton-class)(superclass singleton-class)) t)
(defmethod validate-superclass ((class standard-class)(superclass singleton-class)) nil)

(defmethod make-instance ((class singleton-class) &key)
  (unless (instance class)
    (setf (instance class) (call-next-method)))
  (instance class))

;;; ========================================================================
;;; common operations on standard values
;;; ========================================================================

(defmethod = (x y)(cl:eql x y))

;;; ============================================================
;;; base value types
;;; ============================================================

;;; ------------------------------------------------------------
;;; Nothing
;;; ------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))

(defmethod print-object ((n nothing)(s stream))
  (format s "nothing"))

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

;;; ------------------------------------------------------------
;;; Booleans
;;; ------------------------------------------------------------

(defclass boolean ()())

;;; true

(defclass true (boolean)()(:metaclass singleton-class))

(defmethod print-object ((tt true)(s stream))
  (format s "true"))

(defun true ()(make-instance 'true))

(defmethod true? (x)(declare (ignore x)) nil)
(defmethod true? ((x true))(declare (ignore x)) t)

(defmethod = ((x true) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y true))
  (declare (ignore x))
  nil)

(defmethod = ((x true) (y true))
  (declare (ignore x y))
  t)

;;; false

(defclass false (boolean)()(:metaclass singleton-class))

(defmethod print-object ((f false)(s stream))
  (format s "false"))

(defun false ()(make-instance 'false))

(defmethod false? (x)(declare (ignore x)) nil)
(defmethod false? ((x false))(declare (ignore x)) t)

(defmethod = ((x false) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y false))
  (declare (ignore x))
  nil)

(defmethod = ((x false) (y false))
  (declare (ignore x y))
  t)

;;; ------------------------------------------------------------
;;; Number
;;; ------------------------------------------------------------

(defmethod number? (x) (cl:numberp x))

(defmethod print-value ((n cl:number)(str stream))
  (format str "~A" n))

(defmethod = ((x cl:number) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:number))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:number) (y cl:number))
  (cl:= x y))

;;; ------------------------------------------------------------
;;; Character
;;; ------------------------------------------------------------

(defmethod print-value ((c cl:character)(str stream))
  (format str "\\")
  (write-char c str))

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

(defmethod print-value ((k cl:keyword)(str stream))
  (format str "~A:" (symbol-name k)))

(defmethod keyword ((s cl:string))
  (cl:intern s (find-package :keyword)))

(defmethod keyword? (x)(declare (ignore x)) nil)
(defmethod keyword? ((x cl:keyword))(declare (ignore x)) t)

(defmethod = ((x cl:keyword) y)
  (declare (ignore y))
  nil)

;;; Symbol
;;; ------------------------------------------------------------

(defmethod print-value ((s cl:symbol)(str stream))
  (format str "~A" (symbol-name s)))

(defmethod self-evaluating? ((x cl:symbol))
  (declare (ignore x))
  nil)

(defmethod symbol ((s cl:symbol)) s)

(defmethod symbol ((s cl:string))
  (cl:intern s (find-package "BARD")))

(defmethod symbol? (x)(declare (ignore x)) nil)
(defmethod symbol? ((x cl:symbol))(declare (ignore x)) t)

;;; ============================================================
;;; collections
;;; ============================================================

;;; ------------------------------------------------------------
;;; Sequences
;;; ------------------------------------------------------------

(defmethod print-value ((s cl:cons)(str stream))
  (progn
    (format str "(")
    (let* ((len (length s)))
      (dotimes (i len)
        (when (< 0 i len) (format str " "))
        (print-value (elt s i) str)))
    (format str ")")))

(defmethod sequence? (x)(declare (ignore x)) nil)
(defmethod sequence? ((x cl:cons))(declare (ignore x)) t)

(defun prepend (item seq)
  (cons item seq))

(defun append (seq item)
  (cl:append seq (list item)))

(defun concatenate (seq1 seq2)
  (cl:append seq1 seq2))

(defun |sequence| (&rest items)
  (cl:apply #'cl:list items))

(defmethod = ((x cl:cons) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y cl:cons))
  (declare (ignore x))
  nil)

(defmethod = ((x cl:cons) (y cl:cons))
  (every (lambda (i j) (= i j)) 
               x y))

;;; ------------------------------------------------------------
;;; Text
;;; ------------------------------------------------------------

(defmethod text? (x)(declare (ignore x)) nil)
(defmethod text? ((x cl:string))
  (declare (ignore x))
  t)

(defmethod text ((s cl:string)) s)
(defmethod text ((s cl:list))
  (assert (every 'characterp s)()
          "malformed input to text constructor")
  (coerce s 'string))

;;; ------------------------------------------------------------
;;; Maps
;;; ------------------------------------------------------------

(let ((empty (make-hash-table)))
  (defun empty-map () empty))

(defmethod print-value ((m hash-table)(stream stream))
  (progn
    (format stream "{ ")
    (maphash (lambda (k v)
               (print-object k stream)
               (format stream " ")
               (print-object v stream)
               (format stream " "))
             m)
    (format stream "}")))

(defmethod map? (x)(declare (ignore x)) nil)
(defmethod map? ((x hash-table))(declare (ignore x)) t)

(defun map (&rest entries)
  (if (null entries)
    (empty-map)
    (let ((m (make-hash-table :test 'equal)))
      (do ((items entries (cddr items)))
           ((null items) )
        (setf (gethash (car items) m) (cadr items)))
      m)))

(defmethod get-key ((m hash-table) key &optional (default (nothing)))
  (gethash key m default))

(defmethod = ((x hash-table) y)
  (declare (ignore y))
  nil)

(defmethod = (x (y hash-table))
  (declare (ignore x))
  nil)

(defmethod = ((x hash-table) (y hash-table))
  (maphash (lambda (k v) 
             (equal v (gethash k y)))
           x))
