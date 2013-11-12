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
  (format nil "()"))

(defmethod value->literal-string ((x cons))
  (if (null x)
      "()"
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

(defmethod value->literal-string ((m <alist>))
  (let ((entries (entries m)))
    (if (null entries)
        "{}"
        (with-output-to-string (out)
          (princ #\{ out)
          (let ((e (first entries)))
            (princ (value->literal-string (car e)) out)
            (princ #\space out)
            (princ (value->literal-string (cdr e)) out))
          (dolist (e (rest entries))
            (princ #\space out)
            (princ (value->literal-string (car e)) out)
            (princ #\space out)
            (princ (value->literal-string (cdr e)) out))
          (princ #\} out)))))

(defmethod value->literal-string ((x <true>))
  (format nil "true"))

(defmethod value->literal-string ((x <false>))
  (format nil "false"))

(defmethod value->literal-string ((x <undefined>))
  (format nil "undefined"))

(defmethod value->literal-string ((x symbol))
  (if (keywordp x)
      (format nil "~a:" (symbol-name x))
      (format nil "~a" (symbol-name x))))

(defmethod value->literal-string ((method <method>))
  (let* ((nm (method-name method))
         (name-string (if nm
                          (format nil "name: ~a " 
                                  (if (symbolp nm)
                                      (symbol-name nm)
                                      (format nil "~s" nm)))
                          ""))
         (args (method-args method))
         (args-string (if args
                          (format nil "args: ~a " (value->literal-string args))
                          "args: () "))
         (expr (method-expression method))
         (expr-string (format nil "expression: ~a" (value->literal-string expr))))
    (format nil "#<method>{~a ~a ~a }" name-string args-string expr-string)))

(defmethod value->literal-string ((fn <function>))
  (let* ((input-types (function-input-types fn))
         (input-type-names (mapcar (lambda (tp) (if (symbolp tp) (symbol-name tp) (structure-name tp)))
                                   input-types))
         (inputs-string (if input-type-names
                            (with-output-to-string (out)
                              (format out " ~a" (first input-type-names))
                              (dolist (tp (rest input-type-names))
                                (format out " ~a" tp)))
                            ""))
         (output-types (function-output-types fn))
         (output-type-names (mapcar (lambda (tp) (if (symbolp tp) (symbol-name tp) (structure-name tp)))
                                    output-types))
         (outputs-string (if output-type-names
                             (with-output-to-string (out)
                               (format out " ~a" (first output-type-names))
                               (dolist (tp (rest output-type-names))
                                 (format out " ~a" tp)))
                             "")))
    (with-output-to-string (out)
      (format out "(->~a" inputs-string)
      (format out " ->~a)" outputs-string))))

