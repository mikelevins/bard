;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; reader.lisp
;;;; the bard reader
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


(defparameter eof :|EoF|)
(defun eof-object? (x) (eq x eof))

(defun %%read (&optional (stream *standard-input*))
  (let* ((*readtable* *bard-readtable*))
    (convert-to-bard (read stream nil eof))))

(defun %%read-from-string (s)
  (with-input-from-string (in s)
    (%%read in)))


;;; make sure structured values are converted to proper bard representation
(defun convert-to-bard (x)
  (typecase x
    (cons   (setf (car x) (convert-to-bard (car x)))
            (setf (cdr x) (convert-to-bard (cdr x)))
	    x)
    (symbol (or (convert-named-constant x)
                (convert-number x)
                x))
    (vector (dotimes (i (length x))
              (setf (aref x i) (convert-to-bard (aref x i))))
	    x)
    (fset:map (fset:image (lambda (k v)(values (convert-to-bard x) (convert-to-bard y)))
                          x))
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

(defun convert-named-constant (val)
  (let ((nm (string-upcase (symbol-name val))))
    (cond ((equal nm "NOTHING") nil)
          ((equal nm "TRUE") t)
          ((equal nm "FALSE") nil)
          (t val))))

(defun sign-p (char) (find char "+-"))

;;; ---------------------------------------------------------------------
;;; built-in reader macros
;;; ---------------------------------------------------------------------

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
  #'(lambda (s ignore)(declare (ignore ignore)) (list 'quasiquote (%%read s)))
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
                                  (let ((*read-base* 10)) (%%read stream)))
                              *bard-readtable*)

;;; list literals
;;; ---------------------------------------------------------------------

(set-macro-character #\[
                     (lambda (stream char)
                       (declare (ignore char))
                       (let ((elts (read-delimited-list #\] stream t)))
                         `(bardvm::list ,@elts)))
                     nil
                     *bard-readtable*)


(set-macro-character #\] (get-macro-character #\)) t *bard-readtable*)
(set-syntax-from-char #\] #\) *bard-readtable* *readtable*)

;;; map literals
;;; ---------------------------------------------------------------------

(set-macro-character #\{
                     (lambda (stream char)
                       (declare (ignore char))
                       (let* ((elts (read-delimited-list #\} stream t))
                              (pairs (loop for tail on elts by #'cddr
                                        appending (list (car tail)
                                                        (cadr tail)))))
                         `(bardvm::map ,@pairs)))
                     nil
                     *bard-readtable*)


(set-macro-character #\} (get-macro-character #\)) t *bard-readtable*)
(set-syntax-from-char #\} #\) *bard-readtable* *readtable*)
