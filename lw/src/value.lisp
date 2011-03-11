;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          value.lisp
;;;; Project:       Bard
;;;; Purpose:       base value definitions
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; undefined
;;; ---------------------------------------------------------------------

(defclass undefined ()()(:metaclass singleton-class))
(defun undefined ()(make-instance 'undefined))

(defmethod print-object ((obj undefined)(s stream))
  (princ "undefined" s))

(defmethod defined? (thing) t)
(defmethod defined? ((thing undefined)) nil)

;;; ---------------------------------------------------------------------
;;; nothing
;;; ---------------------------------------------------------------------

(defclass nothing ()()(:metaclass singleton-class))
(defun nothing ()(make-instance 'nothing))

(defmethod print-object ((obj nothing)(s stream))
  (princ "nothing" s))

;;; ---------------------------------------------------------------------
;;; true and false
;;; ---------------------------------------------------------------------

(defclass true ()()(:metaclass singleton-class))
(defun true ()(make-instance 'true))

(defmethod print-object ((obj true)(s stream))
  (princ "true" s))

(defclass false ()()(:metaclass singleton-class))
(defun false ()(make-instance 'false))

(defmethod print-object ((obj false)(s stream))
  (princ "false" s))

;;; ---------------------------------------------------------------------
;;; numbers
;;; ---------------------------------------------------------------------

(defclass integer ()((data :reader data :initarg :data)))

(defmethod print-object ((i integer)(s stream))
  (format s "~A" (data i)))

(defclass float ()((data :reader data :initarg :data)))

(defmethod print-object ((f float)(s stream))
  (format s "~A" (data f)))

;;; ---------------------------------------------------------------------
;;; characters
;;; ---------------------------------------------------------------------

(defclass character ()((data :reader data :initarg :data)))

(defmethod print-object ((ch character)(s stream))
  (princ #\\ s)(princ (print-name-for (data ch)) s))

;;; ---------------------------------------------------------------------
;;; boxes
;;; ---------------------------------------------------------------------

(defclass box ()((value :accessor value :initform (undefined) :initarg :value)))
(defun box (&optional val)
  (make-instance 'box :value (or val (undefined))))

(defmethod get-box ((b box))(value b))
(defmethod set-box! ((b box) val)(setf (value b) val))

(defmethod print-object ((b box)(s stream))
  (princ #\# s)
  (princ #\< s)
  (princ "box " s)
  (print-object (get-box b) s)
  (princ #\> s))

;;; ---------------------------------------------------------------------
;;; names
;;; ---------------------------------------------------------------------

(defclass name ()
  ((module-name :reader module-name :initarg :module-name)
   (variable-name :reader variable-name :initarg :variable-name)))

(defmethod print-object ((nm name)(s stream))
  (when (module-name nm)
    (unless (string= "bard.keyword" (module-name nm))
      (princ (module-name nm) s))
    (princ ":" s))
  (princ (variable-name nm) s))

;;; ---------------------------------------------------------------------
;;; sequences and maps
;;; ---------------------------------------------------------------------

(defclass sequence ()((elements :reader elements :initarg :elements)))

(defun empty-sequence ()(make-instance 'sequence :elements (fset:empty-seq)))

(defmethod print-object ((seq sequence)(s stream))
  (let* ((elts (elements seq))
         (slen (fset:size elts)))
    (princ #\[ s)
    (when (> slen 0)
      (print-object (fset:@ elts 0) s)
      (when (> slen 1)
        (loop for i from 1 below slen
           do (progn
                (princ " " s)
                (print-object (fset:@ elts i) s)))))
    (princ #\] s)))

(defclass application (sequence)())

(defmethod print-object ((seq application)(s stream))
  (let* ((elts (elements seq))
         (slen (fset:size elts)))
    (princ #\( s)
    (when (> slen 0)
      (print-object (fset:@ elts 0) s)
      (when (> slen 1)
        (loop for i from 1 below slen
           do (progn
                (princ " " s)
                (print-object (fset:@ elts i) s)))))
    (princ #\) s)))

(defclass text (sequence)())
(defmethod initialize-instance ((tx text) &rest initargs &key elements &allow-other-keys)
  (if (or (stringp elements)
          (and (or (listp elements)
                   (vectorp elements))
               (every #'characterp elements)))
      (setf (slot-value tx 'elements)
            (fset:convert 'fset:seq elements))
      (error "Text init data missing or malformed: ~s" elements)))

(defmethod print-object ((tx text)(s stream))
  (princ #\" s)
  (fset:do-seq (ch (elements tx))
    (princ ch s))
  (princ #\" s))

(defclass map ()((entries :reader entries)))
(defmethod initialize-instance ((m map) &rest initargs &key entries &allow-other-keys)
  (if (evenp (cl:length entries))
      (let ((entries (loop for (k v . rest) on entries by 'cddr collect (cons k v))))
        (setf (slot-value m 'entries)
              (fset:convert 'fset:map entries)))
      (error "Text init data missing or malformed: ~s" elements)))

(defmethod print-object ((m map)(s stream))
  (princ #\{ s)
  (fset:do-map (k v (entries m))
    (princ " " s)
    (print-object k s)
    (princ " " s)
    (print-object v s))
  (princ " " s)
  (princ #\} s))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(defclass primitive ()
  ((debug-name :reader debug-name :initarg :debug-name)
   (arg-count :reader arg-count :initform nil :initarg :arg-count)
   (code :reader code :initarg :code)))

(defmethod print-object ((p primitive)(s stream))
  (princ "#<primitive " s)
  (princ (debug-name p) s)
  (princ ">" s))

(defclass method ()(debug-name signature code))

(defclass function ()(debug-name signature))

;;; ---------------------------------------------------------------------
;;; types
;;; ---------------------------------------------------------------------

(defclass primitive-type ()(debug-name))

(defclass vector-type ()(debug-name))

(defclass record-type ()(debug-name))

(defclass structure-type ()(debug-name))

