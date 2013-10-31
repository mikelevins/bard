;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          primitives-streams.lisp
;;;; Project:       Bard
;;;; Purpose:       I/O and stream primitives
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; I/O ops
;;; ---------------------------------------------------------------------

(defun display (x) (princ x))
(defun bard-write (x) (princ (value->literal-string x)))
(defun newline () (terpri))

;;; ---------------------------------------------------------------------
;;; streams
;;; ---------------------------------------------------------------------

(defclass <stream> ()
  ((url :accessor stream-url :initform nil :initarg :url)
   (stream :accessor base-stream :initform nil :initarg :stream)))

(defclass <file-stream> (<stream>)
  ((pathname :accessor stream-pathname :initform nil :initarg :pathname)))

;;; ---------------------------------------------------------------------
;;; constructors
;;; ---------------------------------------------------------------------

(defun stream.standard-input ()
  (make-instance '<stream> :url nil :stream cl:*standard-input*))

(defun stream.standard-output ()
  (make-instance '<stream> :url nil :stream cl:*standard-output*))

(defun stream.standard-error ()
  (make-instance '<stream> :url nil :stream cl:*error-output*))

(defmethod open-stream (scheme url direction element-type)
  (error "Unsupported stream specification: ~s"
         (list scheme url direction element-type)))

(defmethod stream.open ((url <url>)(direction symbol)(element-type symbol))
  (open-stream (url.scheme url) url direction element-type))

;;; ---------------------------------------------------------------------
;;; accessors
;;; ---------------------------------------------------------------------

(defmethod close-stream (s)
  (error "Can't close: ~s; it is not an open stream" s))

(defmethod stream.close ((s <stream>))
  (close-stream s))

(defmethod stream.length ((s <stream>)) )

(defmethod stream.read-octet ((in <file-stream>)) )
(defmethod stream.read-octets ((in <file-stream>)(count integer)) )
(defmethod stream.read-all-octets ((in <file-stream>)) )

(defmethod stream.read-character ((in <file-stream>)) )
(defmethod stream.read-characters ((in <file-stream>)(count integer)) )
(defmethod stream.read-all-characters ((in <file-stream>)) )

(defmethod stream.read-line ((in <file-stream>)) )
(defmethod stream.read-lines ((in <file-stream>)(count integer)) )
(defmethod stream.read-all-lines ((in <file-stream>)) )

(defmethod stream.read-object ((in <file-stream>)) )
(defmethod stream.read-objects ((in <file-stream>)(count integer)) )
(defmethod stream.read-all-objects ((in <file-stream>)) )


(defmethod stream.write-octet ((in <file-stream>)(octet integer)) )
(defmethod stream.write-octets ((in <file-stream>) (octets sequence)) )

(defmethod stream.write-character ((in <file-stream>)(ch character)) )
(defmethod stream.write-characters ((in <file-stream>) (characters sequence)) )

(defmethod stream.write-line ((in <file-stream>)(line string)) )
(defmethod stream.write-lines ((in <file-stream>)(lines sequence)) )

(defmethod stream.write-object ((in <file-stream>) object) )
(defmethod stream.write-objects ((in <file-stream>)(objects sequence)) )

;;; ---------------------------------------------------------------------
;;; converters
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; primitive definitions
;;; ---------------------------------------------------------------------

(defprim 'bard-symbols::|display| 1
    (make-prim :name 'bard-symbols::|display|
               :n-args 1
               :opcode 'bard::display
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|newline| 0
    (make-prim :name 'bard-symbols::|newline|
               :n-args 0
               :opcode 'bard::newline
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|read| 0
    (make-prim :name 'bard-symbols::|read|
               :n-args 0
               :opcode 'bard::bard-read
               :always nil
               :side-effects t))

(defprim 'bard-symbols::|write| 1
    (make-prim :name 'bard-symbols::|write|
               :n-args 1
               :opcode 'bard::bard-write
               :always nil
               :side-effects t))
