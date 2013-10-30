;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-alist-map.lisp
;;;; Project:       Bard
;;;; Purpose:       primitives for working with alist-maps
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; <alist-map>
;;; ---------------------------------------------------------------------

(defclass <alist-map> ()
  ((entries :accessor entries :initform nil :initarg :entries)))

(defmethod alist-map? (x)(declare (ignore x)) *false*)
(defmethod alist-map? ((x <alist-map>)) *true*)

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defun alist-map (&rest keys-and-values)
  (let ((entries (loop for (a b) on keys-and-values by #'cddr collecting (cons a b))))
    (make-instance '<alist-map> :entries entries)))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defmethod alist.get ((alist <alist-map>) key &optional (default *undefined*))
  (let ((entry (assoc key (entries alist) :test 'equal)))
    (if entry
        (cdr entry)
        default)))

(defmethod alist.put ((alist <alist-map>) key val)
  (let* ((entries (entries alist))
         (new-entries (remove-if (lambda (e)(equal key (car e)))
                                 entries)))
    (make-instance '<alist-map> :entries (cons (cons key val) new-entries))))

(defmethod alist.keys ((alist <alist-map>))
  (mapcar 'car (entries alist)))

(defmethod alist.vals ((alist <alist-map>))
  (mapcar 'cdr (entries alist)))

(defmethod alist.merge ((alist1 <alist-map>)(alist2 <alist-map>))
  (let* ((entries1 (entries alist1))
         (entries2 (entries alist2))
         (unique1 (set-difference entries1 entries2 :key 'car :test 'equal)))
    (make-instance '<alist-map> :entries (append entries2 unique1))))

;;; ---------------------------------------------------------------------
;;; converters
;;; ---------------------------------------------------------------------

(defmethod as-alist-map ((plist null))
  (make-instance '<alist-map> :entries plist))

(defmethod as-alist-map ((plist cons))
  (apply 'alist-map plist))

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|alist-map?| 1
    (make-prim :name 'bard-symbols::|alist-map?|
               :n-args 1
               :opcode 'bard::alist-map?
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

(defprim 'bard-symbols::|as-alist-map| 1
    (make-prim :name 'bard-symbols::|as-alist-map|
               :n-args 1
               :opcode 'bard::as-alist-map
               :always t
               :side-effects nil))

