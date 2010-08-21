;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          sequences.lisp
;;;; Project:       Bard - a modern Lisp, draft 4
;;;; Purpose:       support for the sequence protocol
;;;;                and data types that support it
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD")

;;; ========================================================================
;;; PROTOCOL: Sequence
;;; ========================================================================

(defgeneric count (seq))

(defgeneric first (seq))
(defgeneric second (seq))
(defgeneric third (seq))
(defgeneric rest (seq))
(defgeneric last (seq))

(defgeneric drop (n seq))
(defgeneric take (n seq))

(defgeneric filter (fn seq))

(defgeneric map-over (fn seq))

(defgeneric fold-left (fn init seq))
(defgeneric fold-right (fn init seq))

(defgeneric prepend (x seq))
(defgeneric append (seq x))
(defgeneric concatenate (seq1 seq2))

;;; ========================================================================
;;; IMPLEMENTATION: fset-sequence
;;; ========================================================================

(defun sequence (&rest elements)
  (if elements
      (fset:convert 'fset:seq elements)
      (fset:empty-seq)))

(defmethod count ((seq fset:seq))
  (fset:size seq))

(defmethod first ((seq fset:seq))
  (or (fset:first seq)
      (nothing)))

(defmethod second ((seq fset:seq))
  (or (fset:lookup seq 1)
      (nothing)))

(defmethod third ((seq fset:seq))
  (or (fset:lookup seq 2)
      (nothing)))

(defmethod rest ((seq fset:seq))
  (fset:less-first seq))

(defmethod last ((seq fset:seq))
  (fset:last seq))

(defmethod drop ((n cl:integer) (seq fset:seq))
  (fset:subseq seq n))

(defmethod take ((n cl:integer) (seq fset:seq))
  (fset:subseq seq 0 n))

(defmethod filter ((fn function) (seq fset:set))
  (fset:filter fn seq))

(defmethod filter ((fn function) (seq fset:seq))
  (fset:filter fn seq))

(defmethod map-over ((fn function) (seq fset:seq))
  (fset:image fn seq))

(defmethod map-over ((fn symbol) (seq fset:seq))
  (fset:image (symbol-function fn) seq))

(defmethod fold-left (fn init seq)
  (fset:reduce fn seq :initial-value init))

(defmethod fold-right (fn init seq)
  (fset:reduce fn seq :from-end t :initial-value init))

(defmethod prepend (x (seq fset:seq))
  (fset:with-first seq x))

(defmethod append ((seq fset:seq) x)
  (fset:with-last seq x))

(defmethod concatenate ((seq1 fset:seq) (seq2 fset:seq))
  (fset:concat seq1 seq2))

