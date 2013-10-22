;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; bard readtable
;;; ---------------------------------------------------------------------

(defparameter *bard-readtable*
  (let ((tbl (reader:copy-readtable)))
    (setf (reader:readtable-case tbl) :preserve)
    tbl))

;;; ---------------------------------------------------------------------
;;; dispatch-macro characters
;;; ---------------------------------------------------------------------

(reader:set-dispatch-macro-character #\# #\d
  ;; In both Common Lisp and Bard,
  ;; #x, #o and #b are hexidecimal, octal, and binary,
  ;; e.g. #xff = #o377 = #b11111111 = 255
  ;; In Bard only, #d255 is decimal 255.
  #'(lambda (stream &rest ignore)
      (declare (ignore ignore))
      (let ((reader:*read-base* 10)) (bard-read stream)))
  *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; bard reader support
;;; ---------------------------------------------------------------------

(defmethod input->value (x) x)

(defmethod input->value ((x symbol)) 
  (cond
    ((eql x '|nothing|) (nothing))
    ((eql x '|true|) (true))
    ((eql x '|false|) (false))
    (t x)))

;;; ---------------------------------------------------------------------
;;; bard read
;;; ---------------------------------------------------------------------

(in-package :reader)

;;; modify the symbol parser from lib/reader.lisp to properly handle
;;; Bard symbols

(defparser parse-symbol-token (token)
  "symbol ::= symbol-name
symbol ::= package-marker symbol-name
symbol ::= package-marker package-marker symbol-name
symbol ::= package-name package-marker symbol-name
symbol ::= package-name package-marker package-marker symbol-name
symbol-name   ::= {alphabetic}+ 
package-name  ::= {alphabetic}+ "
  (let ((colon (position-if
                (lambda (traits) (traitp +ct-package-marker+ traits))
                (token-traits token))))
    (if colon
        (let* ((double-colon (and (< (1+ colon) (token-length token))
                                  (traitp +ct-package-marker+
                                          (token-char-traits token (1+ colon)))))
               (pname (subseq (token-text token) 0 colon))
               (sname (subseq (token-text token)
                              (+ colon (if double-colon 2 1)))))
          (when (position-if
                 (lambda (traits) (traitp +ct-package-marker+ traits))
                 (token-traits token) :start (+ colon (if double-colon 2 1)))
            (reject t "Too many package markers in token ~S" (token-text token)))
          (when (zerop colon)
            ;; Keywords always exist, so let's intern them before finding them.
            (setf pname "KEYWORD")
            (intern sname pname))
          ;; The following form thanks to Andrew Philpot <philpot@ISI.EDU>
          ;; corrects a bug when reading with double-colon uninterned symbols:
          (if (find-package pname)
              (if double-colon
                  (accept 'symbol (intern sname pname))
                  (multiple-value-bind (sym where) (find-symbol sname pname)
                    (if (eq where :external) 
                        (accept 'symbol sym)
                        (reject t "There is no external symbol named ~S in ~
                               the package named ~S" sname pname))))
              (reject t "There is no package with name ~S" pname)))
        ;; no colon in token, let's just intern the symbol in the current package:
        (accept 'symbol (intern (token-text token) *package*)))))

(in-package :bard)

(defun read-with-bard-module-names (stream eof-error-p eof-value recursive-p)
  (reader:read stream eof-error-p eof-value recursive-p))

(defun bard-read (&optional (stream *standard-input*))
  (let ((reader:*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (multiple-value-bind (input mname)(read-with-bard-module-names stream nil (eof) nil)
      (values (input->value input)
              mname))))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

