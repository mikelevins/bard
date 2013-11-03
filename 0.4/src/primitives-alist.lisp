;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-alist.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with alists
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <alist>
;;; ---------------------------------------------------------------------

(defclass <alist> ()
  ((entries :accessor entries :initform nil :initarg :entries)))

(defmethod alist? (x)(declare (ignore x)) *false*)
(defmethod alist? ((x <alist>)) *true*)

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defun alist (&rest keys-and-values)
  (let ((entries (loop for (a b) on keys-and-values by #'cddr collecting (cons a b))))
    (make-instance '<alist> :entries entries)))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defmethod alist.get ((alist <alist>) key &optional (default *undefined*))
  (let ((entry (assoc key (entries alist) :test 'equal)))
    (if entry
        (cdr entry)
        default)))

(defmethod alist.put ((alist <alist>) key val)
  (let* ((entries (entries alist))
         (new-entries (remove-if (lambda (e)(equal key (car e)))
                                 entries)))
    (make-instance '<alist> :entries (cons (cons key val) new-entries))))

(defmethod alist.keys ((alist <alist>))
  (mapcar 'car (entries alist)))

(defmethod alist.vals ((alist <alist>))
  (mapcar 'cdr (entries alist)))

(defmethod alist.merge ((alist1 <alist>)(alist2 <alist>))
  (let* ((entries1 (entries alist1))
         (entries2 (entries alist2))
         (unique1 (set-difference entries1 entries2 :key 'car :test 'equal)))
    (make-instance '<alist> :entries (append entries2 unique1))))

;;; ---------------------------------------------------------------------
;;; converters
;;; ---------------------------------------------------------------------

(defmethod as-alist ((plist null))
  (make-instance '<alist> :entries plist))

(defmethod as-alist ((plist cons))
  (apply 'alist plist))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|alist?| 1
    (make-prim :name 'bard-symbols::|alist?|
               :n-args 1
               :opcode 'bard::alist?
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|alist.get| 2
    (make-prim :name 'bard-symbols::|alist.get|
               :n-args 2
               :opcode 'bard::alist.get
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|alist.put| 3
    (make-prim :name 'bard-symbols::|alist.put|
               :n-args 3
               :opcode 'bard::alist.put
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|alist.keys| 1
    (make-prim :name 'bard-symbols::|alist.keys|
               :n-args 1
               :opcode 'bard::alist.keys
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|alist.vals| 1
    (make-prim :name 'bard-symbols::|alist.vals|
               :n-args 1
               :opcode 'bard::alist.vals
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|alist.merge| 2
    (make-prim :name 'bard-symbols::|alist.merge|
               :n-args 2
               :opcode 'bard::alist.merge
               :always t
               :side-effects nil))

(defprim 'bard-symbols::|as-alist| 1
    (make-prim :name 'bard-symbols::|as-alist|
               :n-args 1
               :opcode 'bard::as-alist
               :always t
               :side-effects nil))

