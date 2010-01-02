;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          runtime.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       functions and data strucrtures used by Bard at runtime
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; Void

(defclass void ()())

(let ((void-instance nil))
  (defmethod make-instance ((c (eql (find-class 'void))) &key)
    (unless void-instance
      (setf void-instance (call-next-method)))
    void-instance))

(defun void () (make-instance 'void))

(defmethod print-object ((obj void) str)
  (format str "void"))

(defmethod void? (x)
  (eql x (void)))

;;; Boolean

(defmethod false? (x)
  (declare (ignore x))
  nil)

(defmethod false? ((x (eql nil)))
  (declare (ignore x))
  t)

(defmethod false? ((x (eql (void))))
  (declare (ignore x))
  t)

(defmethod true? (x)
  (not (false? x)))

;;; Sequence

(defclass sequence ()())

(defclass empty-sequence (sequence)())

(let ((empty-sequence-instance nil))
  (defmethod make-instance ((c (eql (find-class 'empty-sequence))) &key)
    (unless empty-sequence-instance
      (setf empty-sequence-instance (call-next-method)))
    empty-sequence-instance))

(defun empty-sequence () (make-instance 'empty-sequence))

(defmethod first ((x empty-sequence))(void))
(defmethod rest ((x empty-sequence))(empty-sequence))

(defmethod print-object ((obj empty-sequence) str)
  (format str "()"))

(defmethod empty-sequence? (x)
  (eql x (empty-sequence)))

;;; for the moment, sequences are represented as vectors. later,
;;; we'll reimplement them as more efficient immutable structures

(defclass vector-sequence (sequence)
  ((elements :reader elements)))

(defun sequence (&rest elts)
  (if (zerop (length elts))
      (empty-sequence)
      (let ((s (make-instance 'vector-sequence)))
        (setf (slot-value s 'elements) (apply 'vector elts))
        s)))

(defmethod first ((x vector-sequence))
  (elt (elements x) 0))

(defmethod rest ((s vector-sequence))
  (let ((tl (subseq (elements s) 1)))
    (if (zerop (length tl))
        (empty-sequence)
        (let ((s (make-instance 'vector-sequence)))
          (setf (slot-value s 'elements) tl)
          s))))

(defmethod %print-sequence-elements ((s vector-sequence) str)
  (let* ((count (length (elements s)))
         (max-index (- count 1)))
    (dotimes (i max-index)
      (format str "~S " (elt (elements s) i)))
    (if (>= max-index 0)
        (format str "~S" (elt (elements s) max-index)))))

(defmethod print-object ((obj vector-sequence) str)
  (format str "(")
  (%print-sequence-elements obj str)
  (format str ")"))

;;; Map

(defclass map ()())

(defclass empty-map (map)())

(let ((empty-map-instance nil))
  (defmethod make-instance ((c (eql (find-class 'empty-map))) &key)
    (unless empty-map-instance
      (setf empty-map-instance (call-next-method)))
    empty-map-instance))

(defun empty-map () (make-instance 'empty-map))

(defmethod first ((x empty-map))(void))
(defmethod rest ((x empty-map))(empty-map))

(defmethod print-object ((obj empty-map) str)
  (format str "{}"))

(defmethod empty-map? (x)
  (eql x (empty-map)))

;;; for the moment, maps are represented as alists. later,
;;; we'll reimplement them as more efficient immutable structures

(defclass alist-map (map)
  ((entries :reader entries)))

(defun map (&rest keys-and-values)
  (if (zerop (length keys-and-values))
      (empty-map)
      (let ((s (make-instance 'alist-map)))
        (setf (slot-value s 'entries) (util::plist->alist keys-and-values))
        s)))

(defmethod first ((x alist-map))
  (let ((first-entry (car (entries x))))
    (sequence (car first-entry)
              (cdr first-entry))))

(defmethod rest ((s alist-map))
  (let ((rest-entries (cdr (entries s)))
        (rest-map (make-instance 'alist-map)))
    (setf (slot-value rest-map 'entries) rest-entries)
    rest-map))

(defmethod %print-sequence-elements ((s alist-map) str)
  (let* ((count (length (entries s)))
         (max-index (- count 1)))
    (dotimes (i max-index)
      (let ((entry (elt (entries s) i)))
        (format str "~S ~S " (car entry)(cdr entry))))
    (if (>= max-index 0)
        (let ((entry (elt (entries s) max-index)))
          (format str "~S ~S" (car entry)(cdr entry))))))

(defmethod print-object ((obj alist-map) str)
  (format str "{")
  (%print-sequence-elements obj str)
  (format str "}"))
