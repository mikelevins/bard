;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; reader.lisp
;;;; the bard reader
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


(defparameter eof "EoF")
(defun eof-object? (x) (eq x eof))

(defun bard-read (&optional (stream *standard-input*))
  (let* ((*readtable* *bard-readtable*)
         (val (convert-numbers (read stream nil eof))))
    (if (symbolp val)
        (let ((nm (string-upcase (symbol-name val))))
          (cond ((equal nm "NOTHING") nil)
                ((equal nm "TRUE") t)
                ((equal nm "FALSE") nil)
                (t val)))
        val)))

(defun convert-numbers (x)
  "Replace symbols that look like Bard numbers with their values."
  ;; Don't copy structure, make changes in place.
  (typecase x
    (cons   (setf (car x) (convert-numbers (car x)))
            (setf (cdr x) (convert-numbers (cdr x)))
	    x) ; *** Bug fix, gat, 11/9/92
    (symbol (or (convert-number x) x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-numbers (aref x i))))
	    x) ; *** Bug fix, gat, 11/9/92
    (t x)))

(defun convert-number (symbol)
  "If str looks like a complex number, return the number."
  (let* ((str (symbol-name symbol))
         (pos (position-if #'sign-p str))
         (end (- (length str) 1)))
    (when (and pos (char-equal (char str end) #\i))
      (let ((re (read-from-string str nil nil :start 0 :end pos))
            (im (read-from-string str nil nil :start pos :end end)))
        (when (and (numberp re) (numberp im))
          (complex re im))))))

(defun sign-p (char) (find char "+-"))

;;; ---------------------------------------------------------------------
;;; built-in reader macros
;;; ---------------------------------------------------------------------

;;; true and false
;;; ---------------------------------------------------------------------

(set-dispatch-macro-character #\# #\t
  #'(lambda (&rest ignore)(declare (ignore ignore)) t)
  *bard-readtable*)

(set-dispatch-macro-character #\# #\f
  #'(lambda (&rest ignore)(declare (ignore ignore)) nil)
  *bard-readtable*)


;;; quasiquote
;;; ---------------------------------------------------------------------

(defun quasi-q (x)
  "Expand a quasiquote form into append, list, and cons calls."
  (cond
    ((vectorp x)
     (list 'apply 'vector (quasi-q (coerce x 'list))))
    ((atom x)
     (if (constantp x) x (list 'quote x)))
    ((starts-with x 'unquote)
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with x 'quasiquote)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with (first x) 'unquote-splicing)
     (if (null (rest x))
         (second (first x))
         (list 'append (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

(defun combine-quasiquote (left right x)
  "Combine left and right (car and cdr), possibly re-using x."
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list 'quote x)
             (list 'quote (cons (eval left) (eval right)))))
        ((null right) (list 'list left))
        ((starts-with right 'list)
         (list* 'list left (rest right)))
        (t (list 'cons left right))))

(set-macro-character #\`
  #'(lambda (s ignore)(declare (ignore ignore)) (list 'quasiquote (bard-read s)))
  nil *bard-readtable*)

(set-macro-character #\,
                     #'(lambda (stream ignore)
                         (declare (ignore ignore))
                         (let ((ch (read-char stream)))
                           (if (char= ch #\@)
                               (list 'unquote-splicing (read stream))
                               (progn (unread-char ch stream)
                                      (list 'unquote (read stream))))))
                     nil *bard-readtable*)


(setf (get 'quasiquote 'bard-macro)
      #'quasi-q)

;;; decimal numbers
;;; ---------------------------------------------------------------------

(set-dispatch-macro-character #\# #\d
                              ;; In both Common Lisp and Bard,
                              ;; #x, #o and #b are hexidecimal, octal, and binary,
                              ;; e.g. #xff = #o377 = #b11111111 = 255
                              ;; In Bard only, #d255 is decimal 255.
                              #'(lambda (stream &rest ignore)
                                  (declare (ignore ignore))
                                  (let ((*read-base* 10)) (bard-read stream)))
                              *bard-readtable*)

;;; seqs
;;; ---------------------------------------------------------------------

(set-macro-character #\[
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         (fset::convert 'fset:seq elts)))
                     nil
                     *bard-readtable*)


(set-macro-character #\] (get-macro-character #\)) t *bard-readtable*)
(set-syntax-from-char #\] #\) *bard-readtable* *readtable*)

;;; maps
;;; ---------------------------------------------------------------------

(set-macro-character #\{
                     (lambda (stream char)
                       (declare (ignore char))
                       (let* ((elts (read-delimited-list #\} stream t))
                              (pairs (loop for tail on elts by #'cddr
                                        collect (cons (car tail)
                                                      (cadr tail)))))
                         (fset::convert 'fset:map pairs)))
                     nil
                     *bard-readtable*)


(set-macro-character #\} (get-macro-character #\)) t *bard-readtable*)
(set-syntax-from-char #\} #\) *bard-readtable* *readtable*)
