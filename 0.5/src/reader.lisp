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

(rename-package :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER
                :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER
                '(:reader))

(in-package :COM.INFORMATIMAGO.COMMON-LISP.LISP-READER.READER)

;;; modify the symbol-reader to handle Bard syntax

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
                        (accept 'symbol
                                (restart-case (error 'symbol-missing-in-package-error
                                                     :stream *input-stream* :package-name pname :symbol-name sname)
                                  (make-symbol (&rest rest)
                                    :report "Make the missing symbol in the specified package"
                                    (declare (ignore rest))
                                    (intern sname pname)))))))
              (accept 'symbol
                      (restart-case (error 'symbol-in-missing-package-error
                                           :stream *input-stream* :package-name pname :symbol-name sname)
                        (intern-here (&rest rest)
                          :report "Intern the symbol in the current package, instead"
                          (declare (ignore rest))
                          (intern sname))
                        (return-uninterned (&rest rest)
                          :report "Return an uninterned symbol, instead"
                          (declare (ignore rest))
                          (make-symbol sname))))))
        ;; no colon in token, let's just intern the symbol in the current package :
        (accept 'symbol (intern (token-text token) *package*)))))

(in-package :bard)

(defparameter *standard-read-table* (reader:copy-readtable))
(defparameter *bard-read-table* (reader:copy-readtable *standard-read-table*))

;;; bard is case-preserving

(setf (reader:readtable-case *bard-read-table*) :preserve)

;;; map and sequence readers

(reader:set-macro-character #\[
                            (lambda (stream char)
                              (let ((elts (read-delimited-list #\] stream t)))
                                `(fset:seq ,@elts)))
                            *bard-read-table*)

(reader:set-macro-character #\] (reader:get-macro-character #\)))

(reader:set-macro-character #\{
                            (lambda (stream char)
                              (let ((elts (read-delimited-list #\} stream t)))
                                `(fset:convert 'fset:wb-map 
                                               (loop for tail on (cl:list ,@elts) by #'cddr collect (cons (car tail)(cadr tail))))))
                            *bard-read-table*)

(reader:set-macro-character #\} (reader:get-macro-character #\)))

(defun bard-read (&optional input-stream eof-error-p eof-value recursive-p)
  (let ((reader:*readtable* *bard-read-table*))
    (reader:read input-stream eof-error-p eof-value recursive-p)))

(defun bard-read-from-string (string &optional eof-error-p eof-value &key (start 0) end preserve-whitespace)
  (let ((reader:*readtable* *bard-read-table*))
    (reader:read-from-string string eof-error-p eof-value :start start :end end :preserve-whitespace preserve-whitespace)))

;;; (bard-read-from-string "[1 (+ 2 3) 3]")
;;; (bard-read-from-string "{'a 1 'b 2}")
;;; (bard-read-from-string "nothing")
;;; (bard-read-from-string "true")


