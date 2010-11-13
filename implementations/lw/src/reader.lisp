;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2010 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package "BARD-READER")


;;; ---------------------------------------------------------------------
;;; translation tables for bard characters
;;; ---------------------------------------------------------------------

(defclass translation-table ()
  ((char->bard-char :accessor char->bard-char :initform {})
   (bard-char->-char :accessor bard-char->char :initform {})))

(defmethod define-char-translation ((tbl translation-table)(from character)(to character))
  (setf (char->bard-char tbl)(map:merge (char->bard-char tbl) {from to}))
  (setf (bard-char->-char tbl)(map:merge (bard-char->-char tbl) {to from}))
  to)

(defmethod translate-character ((tbl translation-table) char &key default)
  (map:get (char->bard-char tbl) char :default (or default char)))

(defmethod untranslate-character ((tbl translation-table) char &key default)
  (map:get (bard-char->char tbl) char :default (or default char)))

;;; ---------------------------------------------------------------------
;;; bard reader character input stream
;;; ---------------------------------------------------------------------

;;; a bard-reader-character-input-stream reads characters as-needed from
;;; an input character-stream. the characters that have been read so far are
;;; cached in the character cache. the stream performs substitutions as-needed
;;; on input characters, as specified by the translation-table
(defclass bard-reader-character-input-stream (stream:fundamental-character-input-stream)
  ((input-stream :reader input-stream :initarg :input-stream)
   (translation-table :reader translation-table :initform (make-instance 'translation-table))
   (character-cache :reader character-cache 
                    :initform (make-array 1024
                                          :element-type 'character
                                          :adjustable t
                                          :fill-pointer 0))))

(defmethod stream-element-type ((stream bard-reader-character-input-stream)) 'character)
(defmethod input-stream-p ((stream bard-reader-character-input-stream)) t)
(defmethod output-stream-p ((stream bard-reader-character-input-stream)) nil)

(defmethod stream:stream-read-char ((stream bard-reader-character-input-stream))
  (let* ((char (read-char (input-stream stream) nil :eof))
         (translated-char (translate-character (translation-table stream) char)))
    (vector-push-extend translated-char (character-cache stream))
    translated-char))

(defmethod stream:stream-unread-char ((stream bard-reader-character-input-stream) char)
  (stream:stream-unread-char (input-stream stream) 
                             (untranslate-character (translation-table stream) char)))



;;; ---------------------------------------------------------------------
;;; token stream
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; the Bard reader
;;; ---------------------------------------------------------------------

(defmethod read ((in stream))
  )

(defmethod read ((ins string))
  (with-input-from-string (in ins)
    (read in)))

#|

(with-input-from-string (in "abcdefghij")
  (let* ((str (make-instance 'bard-reader-character-input-stream :input-stream in))
         (v (make-array 10 :element-type 'character)))
    (read-sequence v str)
    v))

(read "nothing")
(read "true")
(read "false")
(read "10")
(read "12.34")
(read "\\space")
(read "\\A")
(read "\\9")
(read ":Foo")
(read "Foo:")
(read "Foo:Bar")
(read "baZZ")
(read "()")
(read "[]")
(read "(0 1 2 3 4 5)")
(read "[0 1 2 3 4 5]")
(read "\"\"")
(read "\"Hello!\"")
(read "{ }")
(read "{ greeting \"Hello!\"}")


|#