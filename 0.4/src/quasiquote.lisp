;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          quasiquote.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of quasiquote and friends
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; reader macros
;;; ---------------------------------------------------------------------

(reader:set-macro-character #\` 
  #'(lambda (s ignore)
      (declare (ignore ignore))
      (list '|quasiquote| (bard-read s))) 
  nil *bard-readtable*)

(reader:set-macro-character #\, 
   #'(lambda (stream ignore)
       (declare (ignore ignore))
       (let ((ch (read-char stream)))
         (if (char= ch #\@)
             (list '|unquote-splicing| (reader:read stream))
             (progn (unread-char ch stream)
                    (list '|unquote| (reader:read stream))))))
   nil *bard-readtable*)

;;; ---------------------------------------------------------------------
;;; quasiquote implementation
;;; ---------------------------------------------------------------------

(defun combine-quasiquote (left right x)
  (cond ((and (constantp left) (constantp right))
         (if (and (eql (eval left) (first x))
                  (eql (eval right) (rest x)))
             (list '|quote| x)
             (list '|quote| (cons (eval left) (eval right)))))
        ((null right) (list '|list| left))
        ((starts-with? right '|list|)
         (list* '|list| left (rest right)))
        (t (list '|pair| left right))))

(defun quasi-q (x)
  (cond
    ((atom x)
     (if (constantp x) x (list '|quote| x)))
    ((starts-with? x '|unquote|)      
     (assert (and (rest x) (null (rest2 x))))
     (second x))
    ((starts-with? x '|quasiquote|)
     (assert (and (rest x) (null (rest2 x))))
     (quasi-q (quasi-q (second x))))
    ((starts-with? (first x) '|unquote-splicing|)
     (if (null (rest x))
         (second (first x))
         (list '|append| (second (first x)) (quasi-q (rest x)))))
    (t (combine-quasiquote (quasi-q (car x))
                           (quasi-q (cdr x))
                           x))))

;;; ---------------------------------------------------------------------
;;; define the quasiquote macro
;;; ---------------------------------------------------------------------

(setf (gethash '|quasiquote| *bard-macroexpanders*)
      #'quasi-q)


