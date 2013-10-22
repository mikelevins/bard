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
  (let ((txt (token-text token))
        (colon (position-if
                (lambda (traits) (traitp +ct-package-marker+ traits))
                (token-traits token))))
    (if colon
        (if (= colon (- (length txt) 1))
            (let* ((symname (subseq txt 0 (- (length txt) 1)))
                   (sym (intern symname (find-package :bard-keyword))))
              (setf (get sym 'bard::module) 'bard-modules::|bard.keyword|)
              (accept 'symbol sym))
            (let ((colon2 (position-if (lambda (ch)(char= ch #\:))
                                       txt :start (1+ colon))))
              (if colon2
                  (reject t "Too many colons in symbol name ~S" txt)
                  (let* ((mname (subseq txt 0 colon))
                         (sname (subseq txt (1+ colon)))
                         (sym (intern sname *package*)))
                    (setf (get sym 'bard::module) (intern mname (find-package :bard-modules)))
                    (accept 'symbol sym)))))
        ;; no colon in token, let's just intern the symbol in the current package:
        (let ((sym (intern (token-text token) *package*)))
          (setf (get sym 'bard::module) (bard::current-module-name))
          (accept 'symbol sym)))))

(in-package :bard)

(defun bard-read (&optional (stream *standard-input*))
  (let ((reader:*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (input->value (reader:read stream nil (eof) nil))))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

