;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.lisp
;;;; Project:       Bard
;;;; Purpose:       Bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:bard)

(defmethod print (x (out (eql t)))
  (print x *standard-output*))

(defmethod print ((object package)(out stream))
  (format out "#<module ~A>" (package-name object)))

(defmethod print ((object null)(out stream))
  (format out "nothing"))

(defmethod print ((object (eql t))(out stream))
  (format out "true"))

(defmethod print ((object false)(out stream))
  (format out "false"))

(defmethod print ((object number)(out stream))
  (format out "~S" object))

(defmethod print ((object string)(out stream))
  (format out "~S" object))

(defmethod print ((object symbol)(out stream))
  (let* ((pkg (symbol-package object))
         (pname (cond
                  ((eql pkg (find-package "bard.keyword")) ":")
                  ((eql pkg *module*) "")
                  (t (concatenate 'string (package-name pkg) ":"))))
         (sname (symbol-name object)))
    (format out "~A~A" pname sname)))

(defmethod print ((object character)(out stream))
  (format out "\\~A" object))

(defmethod print ((object fset:seq)(out stream))
  (let ((len (seq:length object)))
    (if (zerop len)
        (format out "()")
        (let* ((hd (seq:element object 0))
               (tl (seq:drop 1 object)))
          (format out "(")
          (print hd out)
          (loop for i from 0 below (1- len)
             do
               (format out " ")
               (print (seq:element tl i) out))
          (format out ")")))))

(defmethod print ((object fset:map)(out stream))
  (let* ((keys (as 'fset:seq (map:keys object)))
         (keycount (seq:length keys)))
    (if (zerop keycount)
        (format out "{}")
        (let* ((hd (seq:element keys 0))
               (tl (seq:drop 1 keys)))
          (format out "{")
          (print hd out)
          (format out " ")
          (print (map:get object hd) out)
          (loop for i from 0 below (1- keycount)
             do
               (format out " ")
               (let* ((k (seq:element tl i))
                      (v (map:get object k)))
                 (print k out)
                 (format out " ")
                 (print v out)))
          (format out "}")))))

;;; (terpri)
;;; (print (read "nothing")  t)
;;; (print (read "true") t)
;;; (print (read "false") t)
;;; (print (read "0") t)
;;; (print (read "1.2") t)
;;; (print (read "#b101") t)
;;; (print (read "\"Foo bar\"") t)
;;; (print (read "Frob") t)
;;; (print (read "bard.core:define") t)
;;; (print (read ":name") t)
;;; (print (read "\\c") t)
;;; (print (read "\\U+0041") t)
;;; (print (read "\\space") t)
;;; (print (read "()") t)
;;; (print (read "(foo)") t)
;;; (print (read "[0 1 2]") t)
;;; (print (read "{}") t)
;;; (print (read "{foo bar}") t)
;;; (print (read "{:name \"Fred\" :friends [:wilma :barney :betty]}") t)

