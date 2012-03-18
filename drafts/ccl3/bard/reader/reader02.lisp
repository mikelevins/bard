;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader02.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard reader pass 2: convert abstract syntax to concrete Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; Bard's read function
;;; ----------------------------------------------------------------------

(in-package :bard)

(defun unicode-character-code? (cname)
  (and (char= #\U (elt cname 0))
       (char= #\+ (elt cname 1))))

(defun unicode-character-code->character (cname)
  (let* ((code-str (subseq cname 2))
         (code (parse-integer code-str :radix 16)))
    (code-char code)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter $character-name-table (make-hash-table :test #'equal))
  (setf (gethash "space" $character-name-table) #\space)
  (setf (gethash "return" $character-name-table) #\return)
  (setf (gethash "newline" $character-name-table) #\newline)
  (setf (gethash "tab" $character-name-table) #\tab))

(defun lookup-character-name (cname)
  (gethash cname $character-name-table))

(defun character-name->character (cname)
  (if (unicode-character-code? cname)
      (unicode-character-code->character cname)
      (lookup-character-name cname)))

(defmethod as-bard-value (x)
  (error "unknown syntax type: ~s" x))

(defmethod as-bard-value ((x end-of-sequence))  x)
(defmethod as-bard-value ((x end-of-map))  x)

(defmethod as-bard-value ((x cons))
  (let ((xtype (first x)))
    (ecase xtype
      ((:nothing) (nothing))
      ((:boolean) (ecase (second x)
                    ((:true)(true))
                    ((:false)(false))))
      ((:number) (second x))
      ((:string) (second x))
      ((:symbol) (let* ((mname (second x))
                        (module (if mname
                                    (find-package mname)
                                    *module*))
                        (sname (third x)))
                   (intern sname module)))
      ((:character)(let* ((cname (second x)))
                     (if (= 1 (length cname))
                         (elt cname 0)
                         (character-name->character cname))))
      ((:sequence)(as 'fset:seq (cdr x)))
      ((:sequence-literal)(as 'fset:seq (cdr x)))
      ((:map) (as 'fset:map (as 'map:plist (cdr x)))))))

(defun %primitive-read (in)
  (let ((*readtable* bard::+bard-read-table+)
        (*package* (find-package :bard)))
    (cl:read in nil (end-of-file) nil)))

(defmethod read ((in stream))
  (as-bard-value (syntax-for (%primitive-read in))))

(defmethod read ((in string))
  (with-input-from-string (s in)
	(read s)))

;;; (format t "~%~S" (read "nothing"))
;;; (format t "~%~S" (read "true"))
;;; (format t "~%~S" (read "false"))
;;; (format t "~%~S" (read "0"))
;;; (format t "~%~S" (read "1.2"))
;;; (format t "~%~S" (read "#b101"))
;;; (format t "~%~S" (read "\"Foo bar\""))
;;; (format t "~%~S" (read "Frob"))
;;; (format t "~%~S" (read "bard.core:define"))
;;; (format t "~%~S" (read ":name"))
;;; (format t "~%~S" (read "\\c"))
;;; (format t "~%~S" (read "\\U+0041"))
;;; (format t "~%~S" (read "\\space"))
;;; (format t "~%~S" (read "()"))
;;; (format t "~%~S" (read "(foo)"))
;;; (format t "~%~S" (read "[0 1 2]"))
;;; (format t "~%~S" (read "{}"))
;;; (format t "~%~S" (read "{foo bar}"))
;;; (format t "~%~S" (read "{:name \"Fred\" :friends [:wilma :barney :betty]}"))

