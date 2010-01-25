;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10; Package: asdf -*-
;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.lisp
;;;; Project:       Bard - a near-minimal Cocoa application
;;;; Purpose:       common utilities
;;;; Author:        mikel evins
;;;; Copyright:     2009 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :util)

(defun plist->alist (plist)
  (if (null plist)
      plist
      (let ((k (car plist))
            (tl (cdr plist)))
        (if (null tl)
            (error "Odd number of arguments")
            (let ((v (car tl))
                  (tl (cdr tl)))
              (cons (cons k v)
                    (plist->alist tl)))))))

(defun range (start end)
  (let ((result nil))
    (dotimes (i (- end start))
      (setf result (push (+ start i) result)))
    (reverse result)))

(defun filter (pred items &optional (acc nil))
  (if (null items)
      (reverse acc)
      (if (funcall pred (car items))
          (filter pred (cdr items) (cons (car items) acc))
          (filter pred (cdr items) acc))))