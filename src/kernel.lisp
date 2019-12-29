;;;; kernel.lisp

(in-package #:bard.internal)

;;; the internal Lisp operators that implement the kernel language
;;; that is the target of the bard compiler in compiler.lisp

;;; functions and macros
;;; ---------------------------------------------------------------------
;;; BEGIN &rest exps
;;; CONSTANT c
;;; IF test then else
;;; METHOD params body env
;;; VARIABLE v env
;;; VARIABLE-SET! var val env

