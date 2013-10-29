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

(defparameter *default-readtable* (reader:copy-readtable))

(defparameter *bard-readtable*
  (let ((tbl (reader:copy-readtable *default-readtable*)))
    (setf (reader:readtable-case tbl) :preserve)
    tbl))

;;; ---------------------------------------------------------------------
;;; character names
;;; ---------------------------------------------------------------------

(defparameter *character-name-table* (make-hash-table :test 'equalp))

(defmethod define-character-name ((cid string)(ch cl:character))
  (setf (gethash cid *character-name-table*) ch))

(defmethod find-character ((cid string))
  (gethash cid *character-name-table* nil))

(progn
    (define-character-name "space" #\space)
    (define-character-name "tab" #\tab)
    (define-character-name "return" #\return)
    (define-character-name "newline" #\newline))

;;; ---------------------------------------------------------------------
;;; special reader values
;;; ---------------------------------------------------------------------

(defclass eof ()())
(defparameter $eof (make-instance 'eof))
(defmethod eof? (x)(declare (ignore x)) nil)
(defmethod eof? ((e eof))(declare (ignore e)) t)

(defclass end-of-list ()())
(defparameter $end-of-list (make-instance 'end-of-list))
(defmethod end-of-list? (x)(declare (ignore x)) nil)
(defmethod end-of-list? ((e end-of-list))(declare (ignore e)) t)

(defclass end-of-map ()())
(defparameter $end-of-map (make-instance 'end-of-map))
(defmethod end-of-map? (x)(declare (ignore x)) nil)
(defmethod end-of-map? ((e end-of-map))(declare (ignore e)) t)

;;; ---------------------------------------------------------------------
;;; dispatch-macro characters
;;; ---------------------------------------------------------------------

(reader:set-macro-character #\\ 
                            (lambda (stream char)
                              (let* ((char-sym (reader:read stream))
                                     (digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
                                (cond
                                  ((eof? char-sym) (error "Unexpected end of input while reading a character"))
                                  ((and (typep char-sym 'cl:number)
                                        (<= 0 char-sym 9))
                                   (elt digits char-sym))
                                  ((typep char-sym 'cl:symbol)
                                   (let ((char-name (cl:symbol-name char-sym)))
                                     (if (= 1 (length char-name))
                                         (character (elt char-name 0))
                                         (or (find-character char-name)
                                             (error "Invalid character syntax: ~x~S" char char-sym)))))
                                  (t (error "Invalid character syntax: ~S" char-sym)))))
                            nil *bard-readtable*)

(defun construct-list (elts)
  (if (null elts)
      nil
      `(bard-symbols::|cons| ,(car elts) ,(construct-list (cdr elts)))))

(defun read-app-form (stream char)
  (declare (ignore char))
  (let ((elements '()))
    (block reading
      (loop
         (let ((next-elt (bard-read stream)))
           (cond
             ((eof? next-elt)(error "Unexpected end of input while reading a list"))
             ((end-of-list? next-elt) (return-from reading (reverse elements)))
             (t (progn (setf elements (cons next-elt elements))))))))))

(defun read-list-form (stream char)
  (declare (ignore char))
  (let ((elements '()))
    (block reading
      (loop
         (let ((next-elt (bard-read stream)))
           (cond
             ((eof? next-elt)(error "Unexpected end of input while reading a list"))
             ((end-of-list? next-elt) (let ((elts (reverse elements)))
                                        (return-from reading (construct-list elts))))
             (t (progn (setf elements (cons next-elt elements))))))))))

(reader:set-macro-character #\[
                            #'read-list-form
                            nil *bard-readtable*)

(reader:set-macro-character #\]
                            (lambda (stream char)
                              (declare (ignore stream char))
                              $end-of-list)
                            nil *bard-readtable*)


(reader:set-macro-character #\(
                            #'read-app-form
                            nil *bard-readtable*)

(reader:set-macro-character #\)
                            (lambda (stream char)
                              (declare (ignore stream char))
                              $end-of-list)
                            nil *bard-readtable*)

;;; quote reader
(defparameter *quote-reader* (reader:get-macro-character #\' *default-readtable*))

(reader:set-macro-character #\'
                            (lambda (stream char)
                              (let ((input (funcall *quote-reader* stream char)))
                                (if (and (listp input)
                                         (eql  'cl:quote
                                               (first input)))
                                    (list 'bard-symbols::|quote| (second input))
                                    input)))
                            nil *bard-readtable*)

(defun read-map-form (stream char)
  (declare (ignore char))
  (let ((elements '()))
    (block reading
      (loop
         (let ((next-elt (bard-read stream)))
           (cond
             ((eof? next-elt)(error "Unexpected end of input while reading a map"))
             ((end-of-map? next-elt) (return-from reading 
                                       (cl:apply 'make-map (reverse elements))))
             (t (progn
                  (setf elements (cons next-elt elements))))))))))

(reader:set-macro-character #\{
                            #'read-map-form
                            nil *bard-readtable*)

(reader:set-macro-character #\}
                            (lambda (stream char)
                              (declare (ignore stream char))
                              $end-of-map)
                            nil *bard-readtable*)


;;; ---------------------------------------------------------------------
;;; bard reader support
;;; ---------------------------------------------------------------------

(defmethod input->value (x) x)

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
                   (sym (intern symname :bard-keywords)))
              (accept 'keyword sym))
            (let ((colon2 (position-if (lambda (ch)(char= ch #\:))
                                       txt :start (1+ colon))))
              (if colon2
                  (reject t "Too many colons in symbol name ~S" txt)
                  (let* ((mname (subseq txt 0 colon))
                         (sname (subseq txt (1+ colon)))
                         (sym (bard::bard-intern sname mname)))
                    (accept 'symbol sym)))))
        ;; no colon in token; intern it in the current module
        (let ((tx (token-text token)))
          (cond
          ((equal tx "undefined") (accept 'symbol (bard::undefined)))
          ((equal tx "nothing") (accept 'symbol (bard::nothing)))
          ((equal tx "true") (accept 'symbol (bard::true)))
          ((equal tx "false") (accept 'symbol (bard::false)))
          (t (let ((sym (bard::bard-intern tx bard::*module*)))
               (accept 'symbol sym))))))))

(defparser parse-symbol-token (token)
  (let ((tx (token-text token)))
    (if (char= #\: (elt tx (- (length tx) 1)))
        (let* ((symname (subseq tx 0 (- (length tx) 1)))
               (sym (intern symname :keyword)))
          (accept 'keyword sym))
        (cond
          ((equal tx "undefined") (accept 'symbol (bard::undefined)))
          ((equal tx "nothing") (accept 'symbol (bard::nothing)))
          ((equal tx "true") (accept 'symbol (bard::true)))
          ((equal tx "false") (accept 'symbol (bard::false)))
          (t (let ((sym (intern tx :bard-symbols)))
               (accept 'symbol sym)))))))

(in-package :bard)

(defun bard-read (&optional (stream *standard-input*))
  (let ((reader:*readtable* *bard-readtable*)
        (*package* (find-package :bard)))
    (input->value (reader:read stream nil (eof) nil))))

(defun bard-read-from-string (s)
  (with-input-from-string (in s)
    (bard-read in)))

