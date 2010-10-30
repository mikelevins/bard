;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard HLVM
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :BARD.READER)

;;; =====================================================================
;;; eof-values
;;; =====================================================================

(defclass eof ()())
(defparameter $eof (make-instance 'eof))
(defmethod eof? (x)(declare (ignore x)) nil)
(defmethod eof? ((e eof))(declare (ignore e)) t)

(defclass end-of-sequence ()())
(defparameter $end-of-sequence (make-instance 'end-of-sequence))
(defmethod end-of-sequence? (x)(declare (ignore x)) nil)
(defmethod end-of-sequence? ((e end-of-sequence))(declare (ignore e)) t)

(defclass end-of-map ()())
(defparameter $end-of-map (make-instance 'end-of-map))
(defmethod end-of-map? (x)(declare (ignore x)) nil)
(defmethod end-of-map? ((e end-of-map))(declare (ignore e)) t)

;;; =====================================================================
;;; bard expressions
;;; =====================================================================

(defclass expression ()())
(defclass nothing-expression (expression)())
(defclass value-expression (expression)
  ((value :reader value :initarg :value)))

(defmethod print-object ((obj value-expression)(str stream))
  (print-unreadable-object (obj str :type t :identity nil)
    (format str "~S" (value obj))))

(defclass numeric-expression (value-expression)())

(defclass character-expression (value-expression)())

;;; ----------------------------------------------------------------------
;;; Bard characters
;;; ----------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *character-name-table* (make-hash-table :test 'equalp)))
  
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod define-character-name ((cid string)(ch cl:character))
    (setf (gethash cid *character-name-table*) ch)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod find-character ((cid string))
    (gethash cid *character-name-table* nil)))
  
  ;; set up some symbolic character names
(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (define-character-name "space" #\space)
    (define-character-name "tab" #\tab)
    (define-character-name "return" #\return)
    (define-character-name "newline" #\newline)))

;;; =====================================================================
;;; bard read table
;;; =====================================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +initial-read-table+ *readtable*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +bard-read-table+ (copy-readtable *readtable*)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (readtable-case +bard-read-table+) :preserve))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-macro-character #\\ 
                       (lambda (stream char)
                         (let* ((char-sym (cl:read stream))
                                (digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                           (cond
                             ((eof? char-sym) (error "Unexpected end of input while reading a character"))
                             ((and (typep char-sym 'number)
                                   (<= 0 char-sym 9))
                              (elt digits char-sym))
                             ((typep char-sym 'symbol)
                              (let ((char-name char-sym))
                                (if (= 1 (length (cl:symbol-name char-name)))
                                    (elt (cl:symbol-name char-sym) 0)
                                    (or (find-character (symbol-name char-sym))
                                        (error "Invalid character syntax: ~x~S" char char-sym)))))
                             (t (error "Invalid character syntax: ~S" char-sym)))))
                       nil +bard-read-table+))

;;; =====================================================================
;;; lisp->bard values
;;; =====================================================================

(defmethod lisp-object->bard-object ((obj (eql 'bard::|nothing|)))
  (make-instance 'nothing-expression))

(defmethod lisp-object->bard-object ((obj number))
  (make-instance 'numeric-expression :value obj))

(defmethod lisp-object->bard-object ((obj number))
  (make-instance 'numeric-expression :value obj))

(defmethod lisp-object->bard-object ((obj character))
  (make-instance 'character-expression :value obj))

;;; =====================================================================
;;; reader
;;; =====================================================================

;;; ---------------------------------------------------------------------
;;; read

(defun read (&optional stream eof-error-p eof-value recursive-p)
  (let ((*readtable* +bard-read-table+)
        (*package* (find-package :bard)))
    (let ((obj (cl:read stream nil $eof nil)))
      (lisp-object->bard-object obj))))

#|
(with-input-from-string (in "nothing")(read in))
(with-input-from-string (in "10")(read in))
(with-input-from-string (in "12.34")(read in))
(with-input-from-string (in "\\space")(read in))
(with-input-from-string (in "\\A")(read in))
(with-input-from-string (in "\\7")(read in))
(with-input-from-string (in ":Foo")(read in))
|#