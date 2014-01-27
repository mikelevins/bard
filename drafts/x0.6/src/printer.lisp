;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard
;;;; Purpose:       the Bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defmethod value->literal-string (x)
  (format nil "~s" x))

(defmethod value->literal-string ((x null))
  (format nil "nothing"))

(defmethod value->literal-string ((x cons))
  (if (null x)
      "nothing"
      (if (consp (cdr x))
          (with-output-to-string (out)
            (princ #\( out)
            (princ (value->literal-string (first x)) out)
            (dolist (arg (rest x))
              (princ #\space out)
              (princ (value->literal-string arg) out))
            (princ #\) out))
          (if (null (cdr x))
              (with-output-to-string (out)
                (princ #\( out)
                (princ (value->literal-string (car x)) out)
                (princ #\) out))
              (with-output-to-string (out)
                (princ #\( out)
                (princ (value->literal-string (car x)) out)
                (princ #\space out)
                (princ #\. out)
                (princ #\space out)
                (princ (value->literal-string (cdr x)) out)
                (princ #\) out))))))

(defmethod value->literal-string ((map fset:wb-map))
  (with-output-to-string (out)
    (princ "{" out)
    (fset:do-map (x y map)
      (write-char #\Space out)
      (princ x out)
      (write-char #\Space out)
      (princ y out))
    (princ " }" out)))

(defmethod value->literal-string ((x <true>))
  (format nil "true"))

(defmethod value->literal-string ((x <false>))
  (format nil "false"))

(defmethod value->literal-string ((x <undefined>))
  (format nil "undefined"))

(defmethod value->literal-string ((x symbol))
  (if (keywordp x)
      (format nil ":~a" (symbol-name x))
      (format nil "~a" (symbol-name x))))

(defmethod bard-print (x &optional (out *standard-output*))
  (princ (value->literal-string x) out))

;;; (init-modules)
;;; (progn (terpri)(bard-print (FSET:SEQ 1 (+ 2 3) 3)))
;;; (progn (terpri)(bard-print (FSET:MAP ('a 1) ('b 2))))
;;; (progn (terpri)(bard-print nil))
;;; (progn (terpri)(bard-print (true)))
;;; (progn (terpri)(bard-print (false)))
;;; (progn (terpri)(bard-print (undefined)))
;;; (progn (terpri)(bard-print (make-instance 'keyword :name "Foo")))
;;; (progn (terpri)(bard-print(bard-read-from-string "bard.user:Foo")))
;;; (progn (terpri)(bard-print(bard-read-from-string "bard.base:+")))
;;; (progn (terpri)(bard-print(bard-read-from-string "+")))
;;; (progn (terpri)(bard-print(bard-read-from-string "#\\C")))
;;; (progn (terpri)(bard-print (bard-read-from-string "#\\space")))
;;; (progn (terpri)(bard-print (bard-read-from-string "#\\1")))
