;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler-pass1-named-constants.lisp
;;;; Project:       Bard
;;;; Purpose:       compiling named constants
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defparameter $named-constants
  '(bard-symbols::|nothing| bard-symbols::|undefined| bard-symbols::|true| bard-symbols::|false| bard-symbols::|eof|))

(defun named-constant? (exp &optional (env nil))
  (member exp $named-constants))

(defun comp1-named-constant (exp &optional (env nil))
  (ecase exp
    (bard-symbols::|nothing| '(%nothing%))
    (bard-symbols::|undefined| '(%undefined%))
    (bard-symbols::|true| '(%true%))
    (bard-symbols::|false| '(%false%))
    (bard-symbols::|eof| '(%eof%))))
