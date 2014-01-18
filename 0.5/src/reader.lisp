;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(eval-when (:compile-toplevel :load-toplevel)
  (rename-package :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER
                  :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER
                  '(:reader)))

(in-package :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER)

;;; modify the symbol-reader to handle Bard syntax

(defun ensure-valid-colons (tx)
  (let ((colon1-pos (position #\: tx :test 'char=)))
    (if colon1-pos
        (let* ((symbol-name (subseq tx (1+ colon1-pos)))
               (colon2-pos (position #\: symbol-name :test 'char=)))
          (if colon2-pos
              (error "Too many colons in a symbol name: ~s" tx)
              t))
        t)))

(defun parse-qualified-symbol (tx)
  (let ((colon-pos (position #\: tx :test 'char=)))
    (if colon-pos
        (let* ((module-name (subseq tx 0 colon-pos))
               (symbol-name (subseq tx (1+ colon-pos))))
          (values module-name symbol-name))
        (values nil tx))))

(defparser parse-symbol-token (token)
  (let ((tx (token-text token)))
    (ensure-valid-colons tx)
    (if (char= #\: (elt tx 0))
        (let* ((symname (subseq tx 1))
               (sym (intern symname :keyword)))
          (accept 'keyword sym))
        (cond
          ((equal tx "undefined") (accept 'symbol 'bard::|undefined|))
          ((equal tx "nothing") (accept 'symbol 'bard::|nothing|))
          ((equal tx "true") (accept 'symbol 'bard::|true|))
          ((equal tx "false") (accept 'symbol 'bard::|false|))
          (t (multiple-value-bind (module-name symbol-name)(parse-qualified-symbol tx)
               (let* ((module (if module-name
                                  (bard::find-module module-name)
                                  bard::*module*))
                      (sym (bard::intern symbol-name module)))
                 (accept 'symbol sym))))))))

(in-package :bard)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *standard-read-table* (reader:copy-readtable))
  (defparameter *bard-read-table* (reader:copy-readtable *standard-read-table*)))

;;; bard is case-preserving

(eval-when (:compile-toplevel :load-toplevel)
  (setf (reader:readtable-case *bard-read-table*) :preserve))

;;; map reader
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (reader:set-syntax-from-char #\{ #\( *bard-read-table* *standard-read-table*)
  (reader:set-syntax-from-char #\} #\) *bard-read-table* *standard-read-table*)

  (reader:set-macro-character #\{
                              (lambda (stream char)
                                (let ((elts (reader:read-delimited-list #\} stream t)))
                                  `(map ,@elts)))
                              t *bard-read-table*))

;;; sequence reader
;;; ---------------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel)
  (reader:set-syntax-from-char #\[ #\( *bard-read-table* *standard-read-table*)
  (reader:set-syntax-from-char #\] #\) *bard-read-table* *standard-read-table*)

  (reader:set-macro-character #\[
                              (lambda (stream char)
                                (let ((elts (reader:read-delimited-list #\] stream t)))
                                  `(fset:seq ,@elts)))
                              t *bard-read-table*))

;;; bard reader
;;; ---------------------------------------------------------------------

(defun bard-read (&optional input-stream eof-error-p eof-value recursive-p)
  (let ((reader:*readtable* *bard-read-table*))
    (reader:read input-stream eof-error-p eof-value recursive-p)))

(defun bard-read-from-string (string &optional eof-error-p eof-value &key (start 0) end preserve-whitespace)
  (let ((reader:*readtable* *bard-read-table*))
    (reader:read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace)))

;;; (init-modules)
;;; (bard-read-from-string "[1 (+ 2 3) 3]")
;;; (bard-read-from-string "{'a 1 'b 2}")
;;; (bard-read-from-string "nothing")
;;; (bard-read-from-string "true")
;;; (bard-read-from-string ":Foo")
;;; (bard-read-from-string "bard.user:Foo")
;;; (bard-read-from-string "bard.base:+")
;;; (bard-read-from-string "+")


